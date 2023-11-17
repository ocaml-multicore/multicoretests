open QCheck
open STM

(** parallel STM tests of Array *)

module AConf =
struct
  type char_bool_fun = (char -> bool) fun_

  let pp_char_bool_fun par fmt f =
    Format.fprintf fmt (if par then "(%s)" else "%s") (Fn.print f)

  type cmd =
    | Length
    | Get of int
    | Set of int * char
    | Sub of int * int
    | Copy
    | Fill of int * int * char
    | To_list
    | For_all of char_bool_fun
    | Exists of char_bool_fun
    | Mem of char
    | Find_opt of char_bool_fun
  (*| Find_index of char_bool_fun since 5.1*)
    | Sort
    | Stable_sort
    | Fast_sort
    | To_seq

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Length -> cst0 "Length" fmt
    | Get x -> cst1 pp_int "Get" par fmt x
    | Set (x, y) -> cst2 pp_int pp_char "Set" par fmt x y
    | Sub (x, y) -> cst2 pp_int pp_int "Sub" par fmt x y
    | Copy -> cst0 "Copy" fmt
    | Fill (x, y, z) -> cst3 pp_int pp_int pp_char "Fill" par fmt x y z
    | To_list -> cst0 "To_list" fmt
    | For_all f -> cst1 pp_char_bool_fun "For_all" par fmt f
    | Exists f -> cst1 pp_char_bool_fun "Exists" par fmt f
    | Mem x -> cst1 pp_char "Mem" par fmt x
    | Find_opt f -> cst1 pp_char_bool_fun "Find_opt" par fmt f
  (*| Find_index f -> cst1 pp_char_bool_fun "Find_index" par fmt f*)
    | Sort -> cst0 "Sort" fmt
    | Stable_sort -> cst0 "Stable_sort" fmt
    | Fast_sort -> cst0 "Fast_sort" fmt
    | To_seq -> cst0 "To_seq" fmt

  let show_cmd = Util.Pp.to_show pp_cmd

  type state = char list
  type sut = char Array.t

  let arb_cmd s =
    let int_gen = Gen.(oneof [small_nat; int_bound (List.length s - 1)]) in
    let char_gen = Gen.printable in
    QCheck.make ~print:show_cmd (*~shrink:shrink_cmd*)
      Gen.(oneof
             [ return Length;
               map (fun i -> Get i) int_gen;
               map2 (fun i c -> Set (i,c)) int_gen char_gen;
               map2 (fun i len -> Sub (i,len)) int_gen int_gen; (* hack: reusing int_gen for length *)
               return Copy;
               map3 (fun i len c -> Fill (i,len,c)) int_gen int_gen char_gen; (* hack: reusing int_gen for length *)
               return To_list;
               map (fun f -> For_all f) (fun1 Observable.char QCheck.bool).gen;
               map (fun f -> Exists f) (fun1 Observable.char QCheck.bool).gen;
               map (fun c -> Mem c) char_gen;
               map (fun f -> Find_opt f) (fun1 Observable.char QCheck.bool).gen;
             (*map (fun f -> Find_index f) (fun1 Observable.char QCheck.bool).gen;*)
               return Sort;
               return Stable_sort;
               return Fast_sort;
               return To_seq;
             ])

  let array_size = 16

  let init_state  = List.init array_size (fun _ -> 'a')

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
    | Sort -> List.sort Char.compare s
    | Stable_sort -> List.stable_sort Char.compare s
    | Fast_sort -> List.fast_sort Char.compare s
    | To_seq -> s

  let init_sut () = Array.make array_size 'a'
  let cleanup _   = ()

  let precond c _s = match c with
    | _ -> true

  let run c a = match c with
    | Length       -> Res (int, Array.length a)
    | Get i        -> Res (result char exn, protect (Array.get a) i)
    | Set (i,c)    -> Res (result unit exn, protect (Array.set a i) c)
    | Sub (i,l)    -> Res (result (array char) exn, protect (Array.sub a i) l)
    | Copy         -> Res (array char, Array.copy a)
    | Fill (i,l,c) -> Res (result unit exn, protect (Array.fill a i l) c)
    | To_list      -> Res (list char, Array.to_list a)
    | For_all (Fun (_,f)) -> Res (bool, Array.for_all f a)
    | Exists (Fun (_,f)) -> Res (bool, Array.exists f a)
    | Mem c        -> Res (bool, Array.mem c a)
    | Find_opt (Fun (_,f)) -> Res (option char, Array.find_opt f a)
  (*| Find_index (Fun (_,f)) -> Res (option int, Array.find_index f a)*)
    | Sort         -> Res (unit, Array.sort Char.compare a)
    | Stable_sort  -> Res (unit, Array.stable_sort Char.compare a)
    | Fast_sort    -> Res (unit, Array.fast_sort Char.compare a)
    | To_seq       -> Res (seq char, List.to_seq (List.of_seq (Array.to_seq a))) (* workaround: Array.to_seq is lazy and will otherwise see and report later Array.set state changes... *)

  let postcond c (s:char list) res = match c, res with
    | Length, Res ((Int,_),i) -> i = List.length s
    | Get i, Res ((Result (Char,Exn),_), r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "index out of bounds")
      else r = Ok (List.nth s i)
    | Set (i,_), Res ((Result (Unit,Exn),_), r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "index out of bounds")
      else r = Ok ()
    | Sub (i,l), Res ((Result (Array Char,Exn),_), r) ->
      if i < 0 || l < 0 || i+l > List.length s
      then r = Error (Invalid_argument "Array.sub")
      else r = Ok (Array.of_list (List.filteri (fun j _ -> i <= j && j <= i+l-1) s))
    | Copy, Res ((Array Char,_),r) -> Array.to_list r = s
    | Fill (i,l,_), Res ((Result (Unit,Exn),_), r) ->
      if i < 0 || l < 0 || i+l > List.length s
      then r = Error (Invalid_argument "Array.fill")
      else r = Ok ()
    | To_list, Res ((List Char,_),cs) -> cs = s
    | For_all (Fun (_,f)), Res ((Bool,_),r) -> r = List.for_all f s
    | Exists (Fun (_,f)), Res ((Bool,_),r) -> r = List.exists f s
    | Mem c, Res ((Bool,_),r) -> r = List.mem c s
    | Find_opt (Fun (_,f)), Res ((Option Char,_),r) -> r = List.find_opt f s
  (*| Find_index (Fun (_,f)), Res ((Option Int,_),r) -> r = List.find_index f s*)
    | Sort, Res ((Unit,_),r) -> r = ()
    | Stable_sort, Res ((Unit,_),r) -> r = ()
    | Fast_sort, Res ((Unit,_),r) -> r = ()
    | To_seq, Res ((Seq Char,_),r) -> Seq.equal (=) r (List.to_seq s)
    | _, _ -> false
end

module ArraySTM_seq = STM_sequential.Make(AConf)
module ArraySTM_dom = STM_domain.Make(AConf)
;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [ArraySTM_seq.agree_test         ~count ~name:"STM Array test sequential";
    ArraySTM_dom.neg_agree_test_par ~count ~name:"STM Array test parallel" (* this test is expected to fail *)
])
