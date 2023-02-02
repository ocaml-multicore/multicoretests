open QCheck
open STM

(** parallel STM tests of Weak arrays *)

module WConf =
struct
  type cmd =
    | Length
    | Set of int * int64 option
    | Get of int
    | Get_copy of int
    | Check of int
    | Fill of int * int * int64 option
  [@@deriving show { with_path = false }]

  type state = int64 option list
  type sut = int64 Weak.t

  let arb_cmd s =
    let int_gen = Gen.(oneof [small_nat; int_bound (List.length s - 1)]) in
    let int64_gen = Gen.(map Int64.of_int small_int) in
    QCheck.make ~print:show_cmd (*~shrink:shrink_cmd*)
      Gen.(oneof
             [ return Length;
               map2 (fun i c -> Set (i,c)) int_gen (option int64_gen);
               map (fun i -> Get i) int_gen;
               map (fun i -> Get_copy i) int_gen;
               map (fun i -> Check i) int_gen;
               map3 (fun i len c -> Fill (i,len,c)) int_gen int_gen (option int64_gen); (* hack: reusing int_gen for length *)
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

  let init_sut () = Weak.create weak_size
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
(*module WeakSTM_dom = STM_domain.Make(WConf)*)
;;
QCheck_base_runner.run_tests_main
  [ WeakSTM_seq.agree_test                ~count:1000 ~name:"STM Weak test sequential";
    (*WeakSTM_dom.neg_agree_test_par        ~count:1000 ~name:"STM Weak test parallel";*)
  ]
