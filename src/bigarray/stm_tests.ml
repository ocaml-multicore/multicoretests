open QCheck
open STM
open Bigarray

(** parallel STM tests of Big Array *)

module BAConf =
struct

  type cmd =
    | Size_in_bytes
    | Get of int
    | Set of int * int
    (* STM don't support bigarray type for the moment*)
    (* | Sub of int * int *)
    | Fill of int

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Size_in_bytes -> cst0 "Size_in_bytes" fmt
    | Get x -> cst1 pp_int "Get" par fmt x
    | Set (x, y) -> cst2 pp_int pp_int "Set" par fmt x y
    | Fill x -> cst1 pp_int "Fill" par fmt x

  let show_cmd = Util.Pp.to_show pp_cmd

  type state = int list
  type sut   = (int, int_elt, c_layout) Array1.t

  let arb_cmd s =
    let int_gen = Gen.(oneof [small_nat; int_bound (List.length s - 1)]) in
    QCheck.make ~print:show_cmd (*~shrink:shrink_cmd*)
      Gen.(oneof
             [ return Size_in_bytes;
               map (fun i -> Get i) int_gen;
               map2 (fun i n -> Set (i,n)) int_gen int_gen;
               (* STM don't support bigarray type for the moment*)
               (* map2 (fun i len -> Sub (i,len)) int_gen int_gen; *)
               map (fun n -> Fill n) int_gen;
             ])

  let barray_size = 16

  let init_state  = List.init barray_size (fun _ -> 0)

  let next_state n s = match n with
    | Size_in_bytes -> s
    | Get _         -> s
    | Set (i,n)     -> List.mapi (fun j n' -> if i=j then n else n') s
    (* STM don't support bigarray type for the moment*)
    (* | Sub (_,_) -> s *)
    | Fill n        -> List.map (fun _ -> n) s

  let init_sut () =
    let ba = Array1.create int C_layout barray_size in
    Array1.fill ba 0 ;
    ba

  let cleanup _ = ()

  let precond _n _s = true

  let run n ba = match n with
    | Size_in_bytes -> Res (STM.int, Array1.size_in_bytes ba)
    | Get i         -> Res (result STM.int exn, protect (Array1.get ba) i)
    | Set (i,n)     -> Res (result unit exn, protect (Array1.set ba i) n)
    (* STM don't support bigarray type for the moment*)
    (* | Sub (i,l)    -> Res (result (array char) exn, protect (Array.sub a i) l) *)
    | Fill n -> Res (result unit exn, protect (Array1.fill ba) n)

  let word_size_in_bytes = Sys.word_size / 8

  let postcond n (s:int list) res = match n, res with
    | Size_in_bytes, Res ((Int,_),r) -> r = word_size_in_bytes * (List.length s)
    | Get i, Res ((Result (Int,Exn),_), r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "index out of bounds")
      else r = Ok (List.nth s i)
    | Set (i,_), Res ((Result (Unit,Exn),_), r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "index out of bounds")
      else r = Ok ()
    (* STM don't support bigarray type for the moment*)
    (* | Sub (i,l), Res ((Result (Array Char,Exn),_), r) ->
      if i < 0 || l < 0 || i+l > List.length s
      then r = Error (Invalid_argument "Array.sub")
      else r = Ok (Array.of_list (List.filteri (fun j _ -> i <= j && j <= i+l-1) s)) *)
    | Fill (_), Res ((Result (Unit,Exn),_), r) -> r = Ok ()
    | _, _ -> false
end

module BigArraySTM_seq = STM_sequential.Make(BAConf)
module BigArraySTM_dom = STM_domain.Make(BAConf)
;;
QCheck_base_runner.run_tests_main [
  BigArraySTM_seq.agree_test         ~count:1000 ~name:"STM BigArray test sequential";
  BigArraySTM_dom.neg_agree_test_par ~count:5000 ~name:"STM BigArray test parallel"
]
