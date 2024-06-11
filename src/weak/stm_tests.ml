open QCheck
open STM

(** parallel STM tests of Weak arrays *)

module WConf =
struct
  type cmd =
    | Length
    | Set of int * data option
    | Get of int
    | Get_copy of int
    | Check of int
    | Fill of int * int * data option
  and data = int64

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Length -> cst0 "Length" fmt
    | Set (x, y) -> cst2 pp_int (pp_option pp_int64) "Set" par fmt x y
    | Get x -> cst1 pp_int "Get" par fmt x
    | Get_copy x -> cst1 pp_int "Get_copy" par fmt x
    | Check x -> cst1 pp_int "Check" par fmt x
    | Fill (x, y, z) ->
        cst3 pp_int pp_int (pp_option pp_int64) "Fill" par fmt x y z

  let show_cmd = Util.Pp.to_show pp_cmd

  type state = data option list
  type sut = data Weak.t

  let shrink_cmd c = match c with
    | Length -> Iter.empty
    | Set (i, d_opt) -> Iter.map (fun i -> Set (i,d_opt)) (Shrink.int i)
    | Get i      -> Iter.map (fun i -> Get i) (Shrink.int i)
    | Get_copy i -> Iter.map (fun i -> Get_copy i) (Shrink.int i)
    | Check i    -> Iter.map (fun i -> Check i) (Shrink.int i)
    | Fill (i,j,d_opt) ->
      Iter.(map (fun i -> Fill (i,j,d_opt)) (Shrink.int i)
            <+>
            map (fun j -> Fill (i,j,d_opt)) (Shrink.int j))

  let arb_cmd s =
    let int_gen = Gen.(oneof [small_nat; int_bound (List.length s - 1)]) in
    let int64_gen = Gen.(map Int64.of_int small_int) in
    QCheck.make ~print:show_cmd ~shrink:shrink_cmd
      Gen.(frequency
             [ 1,return Length;
               1,map2 (fun i c -> Set (i,c)) int_gen (option int64_gen);
               2,map (fun i -> Get i) int_gen;
               2,map (fun i -> Get_copy i) int_gen;
               2,map (fun i -> Check i) int_gen;
               1,map3 (fun i len c -> Fill (i,len,c)) int_gen int_gen (option int64_gen); (* hack: reusing int_gen for length *)
             ])

  let weak_size = 16

  let init_state  = List.init weak_size (fun _ -> None)

  let next_state c s = match c with
    | Length -> s
    | Set (i,c) ->
      List.mapi (fun j c' -> if i=j then c else c') s
    | Get _  -> s
    | Get_copy _  -> s
    | Check _  -> s
    | Fill (i,l,c) ->
      if i >= 0 && l >= 0 && i+l-1 < List.length s
      then
        List.mapi (fun j c' -> if i <= j && j <= i+l-1 then c else c') s
      else s

  let init_sut () = Gc.minor (); Weak.create weak_size
  let cleanup _   = ()

  let precond c _s = match c with
    | _ -> true

  let run c a = match c with
    | Length       -> Res (int, Weak.length a)
    | Set (i,c)    -> Res (result unit exn, protect (Weak.set a i) c)
    | Get i        -> Res (result (option int64) exn, protect (Weak.get a) i)
    | Get_copy i   -> Res (result (option int64) exn, protect (Weak.get_copy a) i)
    | Check i      -> Res (result bool exn, protect (Weak.check a) i)
    | Fill (i,l,c) -> Res (result unit exn, protect (Weak.fill a i l) c)

  let postcond c (s:int64 option list) res = match c, res with
    | Length, Res ((Int,_),i) -> i = List.length s
    | Set (i,_), Res ((Result (Unit,Exn),_), r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "Weak.set")
      else r = Ok ()
    | Get i, Res ((Result (Option Int64,Exn),_), r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "Weak.get")
      else r = Ok (List.nth s i)
    | Get_copy i, Res ((Result (Option Int64,Exn),_), r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "Weak.get_copy")
      else r = Ok (List.nth s i)
    | Check i, Res ((Result (Bool,Exn),_),r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "Weak.check")
      else r = Ok (None <> List.nth s i)
    | Fill (i,l,_), Res ((Result (Unit,Exn),_), r) ->
      if i < 0 || l < 0 || i+l > List.length s
      then r = Error (Invalid_argument "Weak.fill")
      else r = Ok ()
    | _, _ -> false
end

module WeakSTM_seq = STM_sequential.Make(WConf)
module WeakSTM_dom = STM_domain.Make(WConf)


(* Beware: hoop jumping to enable a full major Gc run between the two tests!
   We need that to avoid the state of the second test depending on the resulting
   GC state of the first test and don't want to exit after the first run
   (as QCheck_base_runner.run_tests_main does). *)
let cli_args = QCheck_base_runner.Raw.parse_cli ~full_options:false Sys.argv
let run_tests l =
  QCheck_base_runner.run_tests l
    ~colors:cli_args.cli_colors
    ~verbose:cli_args.cli_verbose
    ~long:cli_args.cli_long_tests ~out:stdout ~rand:cli_args.cli_rand
let status_seq =
  run_tests
    [ WeakSTM_seq.agree_test         ~count:1000 ~name:"STM Weak test sequential"; ]
let () = Gc.full_major ()
let status_par =
  run_tests
    [ WeakSTM_dom.neg_agree_test_par ~count:5000 ~name:"STM Weak test parallel"; ]
let () = Gc.full_major ()
let status_stress =
  run_tests
    [ WeakSTM_dom.stress_test_par    ~count:1000 ~name:"STM Weak stress test parallel"; ]
let _ = exit (if status_seq=0 && status_par=0 && status_stress=0 then 0 else 1)
