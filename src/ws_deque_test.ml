(** Sequential tests of ws_deque *)

open QCheck

(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)

let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
           else z

let work () =
  for _ = 1 to 200 do
    assert (7 = tak 18 12 6);
  done

module WS_deque_conf =
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
          Gen.return Steal;
         ])

  let init_state  = []
  let init_sut () = Ws_deque.M.create ()
  let cleanup _   = ()

  let next_state c s = match c with
    | Is_empty -> s
    | Size     -> s
    | Push i   -> (*i::s*) if i<>1213 then i::s else s (* an artificial fault *)
    | Pop      -> (match s with
        | []    -> s
        | _::s' -> s')
    | Steal    -> (match List.rev s with
        | []    -> s
        | _::s' -> List.rev s')

  let run_cmd c s d = match c with
    | Is_empty  -> (Ws_deque.M.is_empty d) = (s=[])
    | Size      -> (Ws_deque.M.size d) = (List.length s)
    | Push i    -> (Ws_deque.M.push d i; true)
    | Pop       ->
      (try Some (Ws_deque.M.pop d) with Exit -> None)
      = (match s with | [] -> None | j::_ -> Some j)
    | Steal     ->
      (try Some (Ws_deque.M.steal d) with Exit -> None)
      = (match List.rev s with | [] -> None | j::_ -> Some j)

  let precond _ _ = true
end
module WSDT = QCSTM.Make(WS_deque_conf)
;;
QCheck_runner.run_tests_main [WSDT.agree_test ~count:10_000 ~name:"ref-model agreement"]

(*
let test =
  Test.make ~name:"Task.async/await" ~count:100
    (*Non_det.Test.make ~repeat:50 ~name:"Task.async/await" ~count:100*)
    int
    ((*Util.fork_prop_with_timeout 10*)
    (fun _i -> true))
;;
QCheck_base_runner.run_tests_main [test]
*)
(*Non_det.QCheck_runner.run_tests [test]*)
