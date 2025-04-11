open QCheck
open STM

(** parallel STM tests of Array *)

module AConf =
struct
  type elem = int
  type cmd =
    | Length
    | Get of int
    | Set of int * elem
    | Sub of int * int
    | Copy
    | Fill of int * int * elem
    | To_list
    | For_all of (elem -> bool) fun_
    | Exists of (elem -> bool) fun_
    | Mem of elem
    | Find_opt of (elem -> bool) fun_
  (*| Find_index of char_bool_fun since 5.1*)
    | Sort
    | Stable_sort
    | Fast_sort
    | To_seq

  let pp_elem = Util.Pp.pp_int
  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Length -> cst0 "Length" fmt
    | Get x -> cst1 pp_int "Get" par fmt x
    | Set (x, y) -> cst2 pp_int pp_elem "Set" par fmt x y
    | Sub (x, y) -> cst2 pp_int pp_int "Sub" par fmt x y
    | Copy -> cst0 "Copy" fmt
    | Fill (x, y, z) -> cst3 pp_int pp_int pp_elem "Fill" par fmt x y z
    | To_list -> cst0 "To_list" fmt
    | For_all f -> cst1 pp_fun_ "For_all" par fmt f
    | Exists f -> cst1 pp_fun_ "Exists" par fmt f
    | Mem x -> cst1 pp_elem "Mem" par fmt x
    | Find_opt f -> cst1 pp_fun_ "Find_opt" par fmt f
  (*| Find_index f -> cst1 pp_elem_bool_fun "Find_index" par fmt f*)
    | Sort -> cst0 "Sort" fmt
    | Stable_sort -> cst0 "Stable_sort" fmt
    | Fast_sort -> cst0 "Fast_sort" fmt
    | To_seq -> cst0 "To_seq" fmt

  let show_cmd = Util.Pp.to_show pp_cmd

  type state = elem list
  type sut = elem Array.t

  let arb_cmd s =
    let int_gen = Gen.(frequency [1,small_nat; 5,int_bound (List.length s - 1)]) in
    let elem_gen = Gen.(map (fun sh -> 1 lsl sh) (int_bound 62)) in
    QCheck.make ~print:show_cmd (*~shrink:shrink_cmd*)
      Gen.(oneof
             [ return Length;
               map (fun i -> Get i) int_gen;
               map2 (fun i c -> Set (i,c)) int_gen elem_gen;
               map2 (fun i len -> Sub (i,len)) int_gen int_gen; (* hack: reusing int_gen for length *)
               return Copy;
               map3 (fun i len c -> Fill (i,len,c)) int_gen int_gen elem_gen; (* hack: reusing int_gen for length *)
               return To_list;
               map (fun f -> For_all f) (fun1 Observable.int QCheck.bool).gen;
               map (fun f -> Exists f) (fun1 Observable.int QCheck.bool).gen;
               map (fun c -> Mem c) elem_gen;
               map (fun f -> Find_opt f) (fun1 Observable.int QCheck.bool).gen;
             (*map (fun f -> Find_index f) (fun1 Observable.char QCheck.bool).gen;*)
               return Sort;
               return Stable_sort;
               return Fast_sort;
               return To_seq;
             ])

  let array_size = 10

  let init_state  = List.init array_size (fun _ -> 1 (*'a'*))

  let next_state c s = match c with
    | Length -> s
    | Get _  -> s
    | Set (i,c) ->
      List.mapi (fun j c' -> if i=j then c else c') s
    | Sub (_,_) -> s
    | Copy -> s
    | Fill (i,l,c) ->
      if i >= 0 && l >= 0 && i+l-1 < List.length s
      then
        List.mapi (fun j c' -> if i <= j && j <= i+l-1 then c else c') s
      else s
    | To_list -> s
    | For_all _ -> s
    | Exists _ -> s
    | Mem _ -> s
    | Find_opt _ -> s
  (*| Find_index _ -> s*)
    | Sort -> List.sort Int.compare s
    | Stable_sort -> List.stable_sort Int.compare s
    | Fast_sort -> List.fast_sort Int.compare s
    | To_seq -> s

  let init_sut () = Array.make array_size 1 (*'a'*)
  let cleanup _   = ()

  let precond c _s = match c with
    | _ -> true

  let run c a = match c with
    | Length       -> Res (int, Array.length a)
    | Get i        -> Res (result int exn, protect (Array.get a) i)
    | Set (i,c)    -> Res (result unit exn, protect (Array.set a i) c)
    | Sub (i,l)    -> Res (result (array int) exn, protect (Array.sub a i) l)
    | Copy         -> Res (array int, Array.copy a)
    | Fill (i,l,c) -> Res (result unit exn, protect (Array.fill a i l) c)
    | To_list      -> Res (list int, Array.to_list a)
    | For_all (Fun (_,f)) -> Res (bool, Array.for_all f a)
    | Exists (Fun (_,f)) -> Res (bool, Array.exists f a)
    | Mem c        -> Res (bool, Array.mem c a)
    | Find_opt (Fun (_,f)) -> Res (option int, Array.find_opt f a)
  (*| Find_index (Fun (_,f)) -> Res (option int, Array.find_index f a)*)
    | Sort         -> Res (unit, Array.sort Int.compare a)
    | Stable_sort  -> Res (unit, Array.stable_sort Int.compare a)
    | Fast_sort    -> Res (unit, Array.fast_sort Int.compare a)
    | To_seq       -> Res (seq int, List.to_seq (List.of_seq (Array.to_seq a))) (* workaround: Array.to_seq is lazy and will otherwise see and report later Array.set state changes... *)

  let postcond c (s:int list) res = match c, res with
    | Length, Res ((Int,_),i) -> i = List.length s
    | Get i, Res ((Result (Int,Exn),_), r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "index out of bounds")
      else r = Ok (List.nth s i)
    | Set (i,_), Res ((Result (Unit,Exn),_), r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "index out of bounds")
      else r = Ok ()
    | Sub (i,l), Res ((Result (Array Int,Exn),_), r) ->
      if i < 0 || l < 0 || i+l > List.length s
      then r = Error (Invalid_argument "Array.sub")
      else r = Ok (Array.of_list (List.filteri (fun j _ -> i <= j && j <= i+l-1) s))
    | Copy, Res ((Array Int,_),r) -> Array.to_list r = s
    | Fill (i,l,_), Res ((Result (Unit,Exn),_), r) ->
      if i < 0 || l < 0 || i+l > List.length s
      then r = Error (Invalid_argument "Array.fill")
      else r = Ok ()
    | To_list, Res ((List Int,_),cs) -> cs = s
    | For_all (Fun (_,f)), Res ((Bool,_),r) -> r = List.for_all f s
    | Exists (Fun (_,f)), Res ((Bool,_),r) -> r = List.exists f s
    | Mem c, Res ((Bool,_),r) -> r = List.mem c s
    | Find_opt (Fun (_,f)), Res ((Option Int,_),r) -> r = List.find_opt f s
  (*| Find_index (Fun (_,f)), Res ((Option Int,_),r) -> r = List.find_index f s*)
    | Sort, Res ((Unit,_),r) -> r = ()
    | Stable_sort, Res ((Unit,_),r) -> r = ()
    | Fast_sort, Res ((Unit,_),r) -> r = ()
    | To_seq, Res ((Seq Int,_),r) -> Seq.equal (=) r (List.to_seq s)
    | _, _ -> false

  let power_of_2s = Array.init 63 (fun i -> 1 lsl i)
  let is_power_of_2 i = Array.mem i power_of_2s

  let postcond_tear c (s:int list) res = match c, res with
    (* in the below cases we override postcond to check for powers of 2, i.e., no tearing *)
    | Get i, Res ((Result (Int,Exn),_), r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "index out of bounds")
      else
        (match r with
         | Ok i -> is_power_of_2 i
         | Error _ -> false)
    | Sub (i,l), Res ((Result (Array Int,Exn),_), r) ->
      if i < 0 || l < 0 || i+l > List.length s
      then r = Error (Invalid_argument "Array.sub")
      else
        (match r with
         | Ok arr -> Array.for_all is_power_of_2 arr
         | Error _ -> false)
    | Copy, Res ((Array Int,_),r) -> Array.for_all is_power_of_2 r
    | To_list, Res ((List Int,_),cs) -> List.for_all is_power_of_2 cs
    | For_all (Fun (_,_)), Res ((Bool,_),_)
    | Exists (Fun (_,_)), Res ((Bool,_),_)
    | Mem _, Res ((Bool,_),_)
    | Find_opt (Fun (_,_)), Res ((Option Int,_),_) -> true
    | To_seq, Res ((Seq Int,_),r) -> Seq.for_all is_power_of_2 r
    (* otherwise fall back on postcond *)
    | Length, _
    | Set _, _
    | Fill _, _
    | Sort, _
    | Stable_sort, _
    | Fast_sort, _ -> postcond c s res
    | _, _ -> false
end

module ArraySTM_seq = STM_sequential.Make(AConf)
module ArraySTM_dom = STM_domain.Make(AConf)
module ArraySTM_dom_tear = STM_domain.Make(struct include AConf let next_state _ s = s let postcond = postcond_tear end)
;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [(*ArraySTM_seq.agree_test         ~count ~name:"STM Array test sequential";
      ArraySTM_dom.neg_agree_test_par ~count ~name:"STM Array test parallel";*) (* this test is expected to fail *)
    ArraySTM_dom_tear.agree_test_par  ~count ~name:"STM Array test tearing parallel";
])
