open QCheck
open STM

[@@@warning "-27-32-37-26"]

module type Elem = sig
  type t
  val arb : t QCheck.arbitrary
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val equal : t -> t -> bool
  val init_state : t list
  val mapping_fun : t -> t
  val mapping_fun_with_index : int -> t -> t
  val folding_fun : t -> t -> t
  val pred : t -> bool
  val filter_mapping_fun : t -> t option
end

module Dynarray_spec (Elem : Elem) = struct
  type elem = Elem.t

  type _ ty += Elem : elem ty

  let elem : elem ty_show = Elem, Elem.show

  type sut = elem Dynarray.t

  let init_sut () =
    Dynarray.of_list Elem.init_state

  let cleanup _ = ()

  let add_array _ _ =
    assert false

  type idx = I of int [@@unboxed]

  let equal_idx (I i1) (I i2) = Int.equal i1 i2

  type cmd =
    | Get of idx * int
    | Set of idx * int * elem
    | Length of idx
    | Get_last of idx
    | Add_last of idx * elem
    | Append_seq of idx * elem array
    | Pop_last of idx
    | Remove_last of idx
    | Truncate of idx * int
    | Clear of idx
    | Ensure_capacity of idx * int
    | Fit_capacity of idx
    | Set_capacity of idx * int
    | Reset of idx

  let show_cmd : cmd -> string =
    let open Format in
    function
    | Get (I arr_idx, elem_idx) -> sprintf "get (a%d, %d)" arr_idx elem_idx
    | Set (I arr_idx, elem_idx, x) ->
        asprintf "set (a%d, %d, %a)" arr_idx elem_idx Elem.pp x
    | Length (I arr_idx) -> sprintf "length a%d" arr_idx
    | Get_last (I arr_idx) -> sprintf "get_last a%d" arr_idx
    | Add_last (I idx, x) -> asprintf "add_last (a%d, %a)" idx Elem.pp x
    | Append_seq (I idx, arr) ->
        asprintf
          "append_seq (a%d, @[<hov 2>[ %a ]@])"
          idx
          (pp_print_array ~pp_sep:(fun f () -> fprintf f ";@ ") Elem.pp)
          arr
    | Pop_last (I idx) ->
        sprintf "pop_last a%d" idx
    | Remove_last (I arr_idx) -> sprintf "remove_last a%d" arr_idx
    | Truncate (I arr_idx, len) -> sprintf "truncate (a%d, %d)" arr_idx len
    | Clear (I arr_i) -> sprintf "clear a%d" arr_i
    | Ensure_capacity (I arr_idx, n) -> sprintf "ensure_capacity (a%d, %d)" arr_idx n
    | Fit_capacity (I arr_idx) -> sprintf "fit_capacity a%d" arr_idx
    | Set_capacity (I arr_idx, n) -> sprintf "set_capacity (a%d, %d)" arr_idx n
    | Reset (I arr_idx) -> sprintf "reset a%d" arr_idx

  type state = unit

  let shrink_cmd c = match c with
    | Append_seq (i,a) -> Iter.map (fun a -> Append_seq (i,a)) (Shrink.array a)
    | _ -> Iter.empty

  let arb_cmd state : cmd QCheck.arbitrary =
    let open Gen in
    let arr_idx _state = map (fun i -> I i) small_nat in
    let elem = Elem.arb.gen in
    let array elm_gen = Gen.array_size small_nat elm_gen in
    let list elm_gen = Gen.list_size small_nat elm_gen in
    QCheck.make ~print:show_cmd ~shrink:shrink_cmd
      (frequency
        [ (*5, return Create;*)
          (*5, map2 (fun l x -> Make (l, x)) small_nat elem;*)
          50, map2 (fun arr_idx elem_idx -> Get (arr_idx, elem_idx)) (arr_idx state) small_nat;
          50, map3 (fun arr_idx elem_idx x -> Set (arr_idx, elem_idx, x)) (arr_idx state) small_nat elem;
          (*50, map (fun i -> Is_empty i) (arr_idx state);*)
          50, map (fun i -> Length i) (arr_idx state);
          50, map (fun i -> Get_last i) (arr_idx state);
          (*50, map (fun i -> Find_last i) (arr_idx state);*)
          (*5, map (fun i -> Copy i) (arr_idx state);*)
          150, map2 (fun arr_i x -> Add_last (arr_i, x)) (arr_idx state) elem;
          (*
          33, map2 (fun arr_i arr -> Append_array (arr_i, arr)) (arr_idx state) (array elem);
          33, map2 (fun arr_i l -> Append_list (arr_i, l)) (arr_idx state) (list elem);
          33, map2 (fun arr_i1 arr_i2 -> Append (arr_i1, arr_i2)) (arr_idx state) (arr_idx state);
          *)
          50, map2 (fun arr_i arr -> Append_seq (arr_i, arr)) (arr_idx state) (array elem);
          (*
          33, map2 (fun arr_i arr -> Append_iter (arr_i, arr)) (arr_idx state) (array elem);
          50, map (fun arr_i -> Pop_last_opt arr_i) (arr_idx state);
          *)
          50, map (fun arr_i -> Pop_last arr_i) (arr_idx state);
          100, map (fun arr_i -> Remove_last arr_i) (arr_idx state);
          50, map2 (fun arr_i len -> Truncate (arr_i, len)) (arr_idx state) small_nat;
          50, map (fun arr_i -> Clear arr_i) (arr_idx state);
          (*
          5, map (fun i -> Iter i) (arr_idx state);
          5, map (fun i -> Iteri i) (arr_idx state);
          5, map (fun i -> Map i) (arr_idx state);
          5, map (fun i -> Mapi i) (arr_idx state);
          5, map2 (fun init i -> Fold_left (init, i)) elem (arr_idx state);
          5, map2 (fun i init -> Fold_right (i, init)) (arr_idx state) elem;
          50, map (fun i -> Exists i) (arr_idx state);
          50, map (fun i -> For_all i) (arr_idx state);
          5, map (fun i -> Filter i) (arr_idx state);
          5, map (fun i -> Filter_map i) (arr_idx state);
          5, map (fun arr -> Of_array arr) (array elem);
          10, map (fun i -> To_array i) (arr_idx state);
          5, map (fun l -> Of_list l) (list elem);
          10, map (fun i -> To_list i) (arr_idx state);
          5, map (fun arr -> Of_seq arr) (array elem);
          50, map (fun i -> To_seq i) (arr_idx state);
          50, map (fun i -> To_seq_reentrant i) (arr_idx state);
          50, map (fun i -> To_seq_rev i) (arr_idx state);
          50, map (fun i -> To_seq_rev_reentrant i) (arr_idx state);
          50, map (fun i -> Capacity i) (arr_idx state);
          *)
          50, map2 (fun i cap -> Ensure_capacity (i, cap)) (arr_idx state) small_nat;
          (*50, map2 (fun i extra_cap -> Ensure_extra_capacity (i, extra_cap)) (arr_idx state) small_nat;*)
          50, map (fun i -> Fit_capacity i) (arr_idx state);
          100, map2 (fun arr_i cap -> Set_capacity (arr_i, cap)) (arr_idx state) small_nat;
          50, map (fun arr_i -> Reset arr_i) (arr_idx state);
        ])

  let run cmd sut =
    match cmd with
    | Get (arr_i, elem_i) ->
        Res (result elem exn, protect (fun () ->
            let v = Sys.opaque_identity (Dynarray.get sut elem_i) in
            if not (Obj.is_int (Obj.repr (Sys.opaque_identity v))) then raise Exit else v
        ) ())
    | Set (arr_i, elem_i, x) ->
        Res (result unit exn, protect (fun () -> Dynarray.set sut elem_i x) ())
    | Length arr_i ->
        Res (result int exn, protect (fun () -> Dynarray.length sut) ())
    | Get_last arr_i ->
        Res (result elem exn, protect (fun () -> Dynarray.get_last sut) ())
    | Add_last (arr_i, x) ->
        Res (result unit exn, protect (fun () -> Dynarray.add_last sut x) ())
    | Append_seq (arr_i, arr) ->
        Res (result unit exn, protect (fun () -> Dynarray.append_seq sut (Array.to_seq arr)) ())
    | Pop_last arr_i ->
        Res (result elem exn, protect (fun () -> Dynarray.pop_last sut) ())
    | Remove_last arr_i ->
        Res (result unit exn, protect (fun () -> Dynarray.remove_last sut) ())
    | Truncate (arr_i, len) ->
        Res (result unit exn, protect (fun () -> Dynarray.truncate sut len) ())
    | Clear arr_i ->
        Res (result unit exn, protect (fun () -> Dynarray.clear sut) ())
    | Ensure_capacity (arr_i, cap) ->
        Res (result unit exn, protect (fun () -> Dynarray.ensure_capacity sut cap) ())
    | Fit_capacity arr_i ->
        Res (result unit exn, protect (fun () -> Dynarray.fit_capacity sut) ())
    | Set_capacity (arr_i, cap) ->
        Res (result unit exn, protect (fun () -> Dynarray.set_capacity sut cap) ())
    | Reset arr_i ->
        Res (result unit exn, protect (fun () -> Dynarray.reset sut) ())

  let init_state = ()

  module List = struct
    include List

    let[@tail_mod_cons] rec take n =
      function
      | [] -> []
      | _ :: _ when n <= 0 -> []
      | x :: xs -> x :: take (n - 1) xs
  end

  let next_state _cmd _state = ()

  let precond _cmd _state = true

  let postcond : cmd -> state -> res -> bool =
    fun cmd _state res ->
    match cmd, res with
    | Get (arr_i, elem_i), Res ((Result (Elem, Exn), _), res) ->
        (match res with
         | Error Exit -> false
         | _ -> true)
    | _ -> true
end

module Int : Elem = struct
  type t = int
  let arb = QCheck.small_nat
  let pp = Format.pp_print_int
  let equal = Int.equal
  let show = snd STM.int
  let init_state = (*[ [ 1; 2; 3 ]; List.init 12 Fun.id ]*)
    List.init 1024 (Fun.const 0xcafe)
  let mapping_fun = (~-)
  let mapping_fun_with_index i x = i + x
  let folding_fun = (+)
  let pred x = Int.equal 0 x
  let filter_mapping_fun x = if Int.compare x 0 < 0 then Some (-x) else None
end

module Test_domain = struct
  module Int = STM_domain.Make (Dynarray_spec (Int))
end

let () =
  QCheck_base_runner.run_tests_main
    [ (*
      Test_sequential.Int.agree_test       ~count:1_000 ~name:"STM Dynarray test sequential agreement (int)";
      *)
      Test_domain.Int.agree_test_par   ~count:1_000 ~name:"STM Dynarray test parallel (int)";
      (*
      Test_domain.Int.stress_test_par      ~count:1_000 ~name:"STM Dynarray stress test (int)";
      Test_sequential.Float.agree_test     ~count:1_000 ~name:"STM Dynarray test sequential agreement (float)";
      Test_domain.Float.neg_agree_test_par ~count:1_000 ~name:"STM Dynarray test parallel (float)";
      Test_domain.Float.stress_test_par    ~count:1_000 ~name:"STM Dynarray stress test (float)";
      *)
    ]
