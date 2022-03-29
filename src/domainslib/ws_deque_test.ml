(** Sequential tests of ws_deque *)

open QCheck
open STM

module Ws_deque = Domainslib__Ws_deque

(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
(*
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
           else z

let work () =
  for _ = 1 to 200 do
    assert (7 = tak 18 12 6);
  done
 *)
module WSDConf =
struct
  type cmd =
    | Is_empty
    | Size
    | Push of int  (* use int for now *)
    | Pop
    | Steal [@@deriving show { with_path = false }]
  type state = int list
  type sut = int Ws_deque.M.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [Gen.return Is_empty;
          Gen.return Size;
	  Gen.map (fun i -> Push i) int_gen;
          Gen.return Pop;
          (*Gen.return Steal;*) (* No point in stealing from yourself :-D *)
         ])
  let stealer_cmd _s =
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [Gen.return Is_empty;
          Gen.return Size;  (* Size can return -1 when run from another thread *)
          Gen.return Steal;
         ])

  let init_state  = []
  let init_sut () = Ws_deque.M.create ()
  let cleanup _   = ()

  let next_state c s = match c with
    | Is_empty -> s
    | Size     -> s
    | Push i   -> i::s (*if i<>1213 then i::s else s*) (* an artificial fault *)
    | Pop      -> (match s with
        | []    -> s
        | _::s' -> s')
    | Steal    -> (match List.rev s with
        | []    -> s
        | _::s' -> List.rev s')

  let precond _ _ = true

  type res =
    | RIs_empty of bool
    | RSize of int
    | RPush
    | RPop of (int, exn) result
    | RSteal of (int, exn) result [@@deriving show { with_path = false }]

  let run c d = match c with
    | Is_empty -> RIs_empty (Ws_deque.M.is_empty d)
    | Size     -> RSize (Ws_deque.M.size d)
    | Push i   -> (Ws_deque.M.push d i; RPush)
    | Pop      -> RPop (Util.protect Ws_deque.M.pop d)
    | Steal    -> RSteal (Util.protect Ws_deque.M.steal d)

  let postcond c s res = match c,res with
    | Is_empty, RIs_empty b -> b = (s=[])
    | Size,     RSize size  -> (*Printf.printf "size:%i %!" size;*) size = (List.length s)
    | Push _,   RPush       -> true
    | Pop,      RPop res    -> (*Printf.printf "pop:%s %!" (match opt with None -> "None" | Some i -> string_of_int i);*)
                               (match s with | [] -> res = Error Exit | j::_ -> res = Ok j)
    | Steal,    RSteal res  -> (match List.rev s with | [] -> Result.is_error res | j::_ -> res = Ok j)
    | _,_ -> false
end

module WSDT = STM.Make(WSDConf)
(*
module WSDConf = STM.AddGC(WSDConf)
module WSDT = STM.Make(WSDConf)
 *)

(*
(* Note: this can generate, e.g., pop commands/actions in different threads, thus violating the spec. *)
let agree_test_par ~count ~name =
  let seq_len = 20 in
  let par_len = 15 in
  Test.make ~count ~name ~retries:100
    (WSDT.arb_cmds_par seq_len par_len) WSDT.agree_prop_par

;;
QCheck_runner.run_tests ~verbose:true [
    WSDT.agree_test     ~count:1_000 ~name:"sequential ws_deque test";
    WSDT.agree_test_par ~count:1_000 ~name:"parallel ws_deque test (w/repeat)";
    agree_test_par      ~count:1_000 ~name:"parallel ws_deque test (w/non_det module)";
  ]
 *)

(* Triple printer, that prints [owner] on same line as sequential prefix [seq] *)
let print_triple show_elem (seq,owner,stealer) =
  let header1, header2 = "Seq.prefix:", "Parallel procs.:" in
  let pr_cmds = Print.list show_elem in
  let seq_str = pr_cmds seq in
  let seq_len = max (String.length header1) (String.length seq_str) in
  let buf = Buffer.create 64 in
  begin
    Printf.bprintf buf " %-*s  %s\n\n" seq_len header1 header2;
    Printf.bprintf buf " %*s  %s\n\n" seq_len seq_str (pr_cmds owner);
    Printf.bprintf buf " %s  %s\n" (String.make seq_len ' ') (pr_cmds stealer);
    Buffer.contents buf
  end

let agree_prop_par =
  (fun (seq_pref,owner,stealer) ->
    assume (WSDT.cmds_ok WSDConf.init_state (seq_pref@owner));
    assume (WSDT.cmds_ok WSDConf.init_state (seq_pref@stealer));
    let sut = WSDConf.init_sut () in
    let pref_obs = WSDT.interp_sut_res sut seq_pref in
    let wait = Atomic.make true in
    let stealer_dom = Domain.spawn (fun () -> Atomic.set wait false; WSDT.interp_sut_res sut stealer) in
    while Atomic.get wait do Domain.cpu_relax() done;
    let own_obs = WSDT.interp_sut_res sut owner in
    let stealer_obs = Domain.join stealer_dom in
    let res = WSDT.check_obs pref_obs own_obs stealer_obs WSDConf.init_state in
    let () = WSDConf.cleanup sut in
    res ||
      Test.fail_reportf "Result observations not explainable by linearized model:\n\n %s"
      @@ print_triple WSDConf.show_res
           (List.map snd pref_obs,
            List.map snd own_obs,
            List.map snd stealer_obs))

let shrink_triple =
  let (<+>) = Iter.(<+>) in
  (fun (seq,p1,p2) ->
    (Shrink.(triple list list list) (seq,p1,p2))
    <+> (match p1 with [] -> Iter.empty | c1::c1s -> Iter.return (seq@[c1],c1s,p2))
    <+> (match p2 with [] -> Iter.empty | c2::c2s -> Iter.return (seq@[c2],p1,c2s)))

let arb_triple =
  let seq_len,par_len = 20,15 in
  let seq_pref_gen = WSDT.gen_cmds_size WSDConf.init_state (Gen.int_bound seq_len) in
  let triple_gen = Gen.(seq_pref_gen >>= fun seq_pref ->
                        let spawn_state = List.fold_left (fun st c -> WSDConf.next_state c st) WSDConf.init_state seq_pref in
                        let owner_gen = WSDT.gen_cmds_size spawn_state (Gen.int_bound par_len) in
                        let stealer_gen = list_size (int_bound par_len) (WSDConf.stealer_cmd spawn_state).gen in
                        map2 (fun owner stealer -> (seq_pref,owner,stealer)) owner_gen stealer_gen) in
  make ~print:(print_triple WSDConf.show_cmd) ~shrink:shrink_triple triple_gen

(* A parallel agreement test - w/repeat *)
let agree_test_par_repeat ~count ~name =
  let rep_count = (*250*) 50 (*50*) in
  Test.make ~count ~name
    arb_triple (STM.repeat rep_count agree_prop_par)

(* A parallel agreement test - w/retries *)
let agree_test_par_nondet ~count ~name =
  Test.make ~count ~name ~retries:100
    arb_triple agree_prop_par

(* A parallel agreement test - w/repeat and retries combined *)
let agree_test_par_comb ~count ~name =
  let rep_count = 15 in
  Test.make ~retries:15 ~count ~name
    arb_triple (STM.repeat rep_count agree_prop_par) (* 15 times each, then 15 * 15 times when shrinking *)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count,name = 1000,"ws_deque test" in [
    WSDT.agree_test            ~count ~name;
         agree_test_par_repeat ~count ~name:"parallel ws_deque test (w/repeat)";
       (*agree_test_par_nondet ~count ~name:"parallel ws_deque test (w/shrink retries)";
         agree_test_par_comb   ~count ~name:"parallel ws_deque test (w/repeat+retries combined)";*)
  ])
