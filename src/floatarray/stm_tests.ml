open QCheck
open STM

(** parallel STM tests of Float.Array *)

module FAConf =
struct
  type cmd =
    | Length
    | Get of int
    | Set of int * float

    (* STM don't support floatarray type for the moment*)
    (* | Sub of int * int
    | Copy *)

    | Fill of int * int * float
    | To_list
    | Mem of float
    | Sort
    | To_seq

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Length -> cst0 "Length" fmt
    | Get x -> cst1 pp_int "Get" par fmt x
    | Set (x, y) -> cst2 pp_int pp_float "Set" par fmt x y
    | Fill (x, y, z) -> cst3 pp_int pp_int pp_float "Fill" par fmt x y z
    | To_list -> cst0 "To_list" fmt
    | Mem x -> cst1 pp_float "Mem" par fmt x
    | Sort -> cst0 "Sort" fmt
    | To_seq -> cst0 "To_seq" fmt

  let show_cmd = Util.Pp.to_show pp_cmd

  type state = float list
  type sut = Float.Array.t

  let arb_cmd s =
    let int_gen = Gen.(frequency [ (1,small_nat);
                                   (7,int_bound (List.length s - 1)); ]) in
    let float_gen = Gen.float in
    QCheck.make ~print:show_cmd (*~shrink:shrink_cmd*)
      Gen.(oneof
             [ return Length;
               map (fun i -> Get i) int_gen;
               map2 (fun i f -> Set (i,f)) int_gen float_gen;

               (* STM don't support floatarray type for the moment*)
               (* map2 (fun i len -> Sub (i,len)) int_gen int_gen; (* hack: reusing int_gen for length *)
               return Copy; *)

               map3 (fun i len c -> Fill (i,len,c)) int_gen int_gen float_gen; (* hack: reusing int_gen for length *)
               return To_list;
               map (fun f -> Mem f) float_gen;
               return Sort;
               return To_seq;
             ])

  let floatarray_size = 16

  let init_state  = List.init floatarray_size (fun _ -> 1.0)

  let next_state f s = match f with
    | Length -> s
    | Get _  -> s
    | Set (i,f) ->
      List.mapi (fun j f' -> if i=j then f else f') s

    (* STM don't support floatarray type for the moment*)
    (* | Sub (_,_) -> s
    | Copy -> s *)

    | Fill (i,l,f) ->
      if i >= 0 && l >= 0 && i+l-1 < List.length s
      then
        List.mapi (fun j f' -> if i <= j && j <= i+l-1 then f else f') s
      else s
    | To_list -> s
    | Mem _ -> s
    | Sort -> List.sort Float.compare s
    | To_seq -> s

  let init_sut () = Float.Array.make floatarray_size 1.0
  let cleanup _   = ()

  let precond f _s = match f with
    | _ -> true

  let run f fa = match f with
    | Length       -> Res (int, Float.Array.length fa)
    | Get i        -> Res (result float exn, protect (Float.Array.get fa) i)
    | Set (i,f)    -> Res (result unit exn, protect (Float.Array.set fa i) f)

    (* STM don't support floatarray type for the moment*)
    (* | Sub (i,l)    -> Res (result (floatarray) exn, protect (Float.Array.sub fa i) l) *)
    (* | Copy         -> Res (floatarray, Float.Array.copy fa) *)

    | Fill (i,l,f) -> Res (result unit exn, protect (Float.Array.fill fa i l) f)
    | To_list      -> Res (list float, Float.Array.to_list fa)
    | Mem f        -> Res (bool, Float.Array.mem f fa)
    | Sort         -> Res (unit, Float.Array.sort Float.compare fa)
    | To_seq       -> Res (seq float, List.to_seq (List.of_seq (Float.Array.to_seq fa)))

  let postcond f (s:float list) res = match f, res with
    | Length, Res ((Int,_),i) -> i = List.length s
    | Get i, Res ((Result (Float,Exn),_), r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "index out of bounds")
      else r = Ok (List.nth s i)
    | Set (i,_), Res ((Result (Unit,Exn),_), r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "index out of bounds")
      else r = Ok ()

    (* STM don't support floatarray type for the moment*)
    (* | Sub (i,l), Res ((Result (Float.Array Float,Exn),_), r) ->
      if i < 0 || l < 0 || i+l > List.length s
      then r = Error (Invalid_argument "Float.Array.sub")
      else r = Ok (Float.Array.of_list (List.filteri (fun j _ -> i <= j && j <= i+l-1) s)) *)
    (* | Copy, Res ((Float.Array Float,_),r) -> Float.Array.to_list r = s *)

    | Fill (i,l,_), Res ((Result (Unit,Exn),_), r) ->
      if i < 0 || l < 0 || i+l > List.length s
      then r = Error (Invalid_argument "Float.Array.fill")
      else r = Ok ()
    | To_list, Res ((List Float,_),fs) -> List.equal Float.equal fs s
    | Mem f, Res ((Bool,_),r) -> r = List.mem f s
    | Sort, Res ((Unit,_),r) -> r = ()
    | To_seq, Res ((Seq Float,_),r) -> Seq.equal (=) r (List.to_seq s)
    | _, _ -> false
end

module FloatArraySTM_seq = STM_sequential.Make(FAConf)
module FloatArraySTM_dom = STM_domain.Make(FAConf)
;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [FloatArraySTM_seq.agree_test         ~count ~name:"STM Float Array test sequential";
    FloatArraySTM_dom.neg_agree_test_par ~count ~name:"STM Float Array test parallel"
])
