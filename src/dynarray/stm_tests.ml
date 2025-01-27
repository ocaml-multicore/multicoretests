open QCheck
open STM

[@@@warning "-27-32-37-26"]

module type Elem = sig
  type t
  val arb : t QCheck.arbitrary
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val equal : t -> t -> bool
  val init_state : t list list
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

  (* We are plucking from a pool of Dynarrays. New arrays can be added to the
     pool, sometimes arrays can be removed. *)
  type sut = elem Dynarray.t list ref

  let init_sut () =
    ref (List.map Dynarray.of_list Elem.init_state)

  let cleanup _ = ()

  let add_array arr sut =
    sut := arr :: !sut

  type idx = I of int [@@unboxed]

  let equal_idx (I i1) (I i2) = Int.equal i1 i2

  type cmd =
    | Create
    | Make of int * elem
    | Get of idx * int
    | Set of idx * int * elem
    | Length of idx
    | Is_empty of idx
    | Get_last of idx
    | Find_last of idx
    | Copy of idx
    | Add_last of idx * elem
    | Append_array of idx * elem array
    | Append_list of idx * elem list
    | Append of idx * idx
    | Append_seq of idx * elem array
    | Append_iter of idx * elem array
    | Pop_last_opt of idx
    | Remove_last of idx
    | Truncate of idx * int
    | Clear of idx
    | Iter of idx  (* Allocate a short-lived cell for each element *)
    | Iteri of idx  (* Allocate a short-lived cell for each element *)
    | Map of idx  (* Negate all elements *)
    | Mapi of idx  (* Add indices and elements *)
    | Fold_left of elem * idx  (* Sum over elements *)
    | Fold_right of idx * elem  (* Sum over elements *)
    | Exists of idx  (* Predicate: (=) 0. *)
    | For_all of idx  (* Predicate: (=) 0. *)
    | Filter of idx  (* Predicate: (=) 0. *)
    | Filter_map of idx  (* f: fun x -> if x < 0 then Some (-.x) else None *)
    | Of_array of elem array
    | To_array of idx
    | Of_list of elem list
    | To_list of idx
    | Of_seq of elem array
    | To_seq of idx
        (* The produced sequence is turned into a list immediately, see [run]. *)
    | To_seq_reentrant of idx
    | To_seq_rev of idx
    | To_seq_rev_reentrant of idx
    | Capacity of idx
    | Ensure_capacity of idx * int
    | Ensure_extra_capacity of idx * int
    | Fit_capacity of idx
    | Set_capacity of idx * int
    | Reset of idx

  let show_cmd : cmd -> string =
    let open Format in
    function
    | Create -> "create"
    | Make (l, x) -> asprintf "make (%d, %a)" l Elem.pp x
    | Get (I arr_idx, elem_idx) -> sprintf "get (a%d, %d)" arr_idx elem_idx
    | Set (I arr_idx, elem_idx, x) ->
        asprintf "set (a%d, %d, %a)" arr_idx elem_idx Elem.pp x
    | Is_empty (I arr_idx) -> sprintf "is_empty a%d" arr_idx
    | Length (I arr_idx) -> sprintf "length a%d" arr_idx
    | Get_last (I arr_idx) -> sprintf "get_last a%d" arr_idx
    | Find_last (I idx) -> sprintf "find_last a%d" idx
    | Copy (I idx) -> sprintf "copy a%d" idx
    | Add_last (I idx, x) -> asprintf "add_last (a%d, %a)" idx Elem.pp x
    | Append_array (I idx, arr) ->
        asprintf
          "append_array (a%d, @[<hov 2>[| %a |]@])"
          idx
          (pp_print_array ~pp_sep:(fun f () -> fprintf f ";@ ") Elem.pp)
          arr
    | Append_list (I idx, l) ->
        asprintf
          "append_list (a%d, @[<hov 2>[ %a ]@])"
          idx
          (pp_print_list ~pp_sep:(fun f () -> fprintf f ";@ ") Elem.pp)
          l
    | Append (I arr_i1, I arr_i2) -> sprintf "append (a%d, a%d)" arr_i1 arr_i2
    | Append_seq (I idx, arr) ->
        asprintf
          "append_seq (a%d, @[<hov 2>[ %a ]@])"
          idx
          (pp_print_array ~pp_sep:(fun f () -> fprintf f ";@ ") Elem.pp)
          arr
    | Append_iter (I idx, arr) ->
        asprintf
          "append_iter (a%d, @[<hov 2>[| %a |]@])"
          idx
          (pp_print_array ~pp_sep:(fun f () -> fprintf f ";@ ") Elem.pp)
          arr
    | Pop_last_opt (I idx) ->
        sprintf "pop_last_opt a%d" idx
    | Remove_last (I arr_idx) -> sprintf "remove_last a%d" arr_idx
    | Truncate (I arr_idx, len) -> sprintf "truncate (a%d, %d)" arr_idx len
    | Clear (I arr_i) -> sprintf "clear a%d" arr_i
    | Iter (I i) -> sprintf "iter a%d" i
    | Iteri (I i) -> sprintf "iteri a%d" i
    | Map (I i) -> sprintf "map a%d" i
    | Mapi (I i) -> sprintf "mapi a%d" i
    | Fold_left (init, I i) -> asprintf "fold_left (%a, a%d)" Elem.pp init i
    | Fold_right (I i, init) -> asprintf "fold_right (a%d, %a)" i Elem.pp init
    | Exists (I i) -> sprintf "exists a%d" i
    | For_all (I i) -> sprintf "for_all a%d" i
    | Filter (I i) -> sprintf "filter a%d" i
    | Filter_map (I i) -> sprintf "filter_map a%d" i
    | Of_array arr ->
        asprintf
          "of_array @[<hov 2>[| %a |]@]"
          (pp_print_array ~pp_sep:(fun f () -> fprintf f ";@ ") Elem.pp)
          arr
    | To_array (I i) -> sprintf "to_array a%d" i
    | Of_list l ->
        asprintf
          "of_list @[<hov 2>[ %a ]@]"
          (pp_print_list ~pp_sep:(fun f () -> fprintf f ";@ ") Elem.pp)
          l
    | To_list (I i) -> sprintf "to_list a%d" i
    | Of_seq arr ->
        asprintf
          "of_seq @[<hov 2>[| %a |]@]"
          (pp_print_array ~pp_sep:(fun f () -> fprintf f ";@ ") Elem.pp)
          arr
    | To_seq (I i) -> sprintf "to_seq a%d" i
    | To_seq_reentrant (I i) -> sprintf "to_seq_reentrant a%d" i
    | To_seq_rev (I i) -> sprintf "to_seq_rev a%d" i
    | To_seq_rev_reentrant (I i) -> sprintf "to_seq_rev_reentrant a%d" i
    | Capacity (I i) -> sprintf "capacity a%d" i
    | Ensure_capacity (I arr_idx, n) -> sprintf "ensure_capacity (a%d, %d)" arr_idx n
    | Ensure_extra_capacity (I arr_idx, n) ->
        sprintf "ensure_extra_capacity (a%d, %d)" arr_idx n
    | Fit_capacity (I arr_idx) -> sprintf "fit_capacity a%d" arr_idx
    | Set_capacity (I arr_idx, n) -> sprintf "set_capacity (a%d, %d)" arr_idx n
    | Reset (I arr_idx) -> sprintf "reset a%d" arr_idx

  type state = unit

  let shrink_cmd c = match c with
    | Append_array (i,a) -> Iter.map (fun a -> Append_array (i,a)) (Shrink.array a)
    | Append_list (i,l) -> Iter.map (fun l -> Append_list (i,l)) (Shrink.list l)
    | Append_seq (i,a) -> Iter.map (fun a -> Append_seq (i,a)) (Shrink.array a)
    | Append_iter (i,a) -> Iter.map (fun a -> Append_iter (i,a)) (Shrink.array a)
    | Of_array a -> Iter.map (fun a -> Of_array a) (Shrink.array a)
    | Of_list l -> Iter.map (fun l -> Of_list l) (Shrink.list l)
    | Of_seq a -> Iter.map (fun a -> Of_seq a) (Shrink.array a)
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
          50, map2 (fun arr_i x -> Add_last (arr_i, x)) (arr_idx state) elem;
          (*
          33, map2 (fun arr_i arr -> Append_array (arr_i, arr)) (arr_idx state) (array elem);
          33, map2 (fun arr_i l -> Append_list (arr_i, l)) (arr_idx state) (list elem);
          33, map2 (fun arr_i1 arr_i2 -> Append (arr_i1, arr_i2)) (arr_idx state) (arr_idx state);
          *)
          33, map2 (fun arr_i arr -> Append_seq (arr_i, arr)) (arr_idx state) (array elem);
          (*
          33, map2 (fun arr_i arr -> Append_iter (arr_i, arr)) (arr_idx state) (array elem);
          *)
          50, map (fun arr_i -> Pop_last_opt arr_i) (arr_idx state);
          50, map (fun arr_i -> Remove_last arr_i) (arr_idx state);
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
          50, map2 (fun arr_i cap -> Set_capacity (arr_i, cap)) (arr_idx state) small_nat;
          33, map (fun arr_i -> Reset arr_i) (arr_idx state);
        ])

  let run cmd sut =
    let nth sut (I idx) = List.nth !sut idx in
    match cmd with
    | Create -> Res (unit, add_array (Dynarray.create ()) sut)
    | Make (l, x) -> Res (unit, add_array (Dynarray.make l x) sut)
    | Get (arr_i, elem_i) ->
        Res (result elem exn, protect (fun () ->
            let v = Dynarray.get (nth sut arr_i) elem_i in
            if not (Obj.is_int (Obj.repr v)) then raise Exit else v
        ) ())
    | Set (arr_i, elem_i, x) ->
        Res (result unit exn, protect (fun () -> Dynarray.set (nth sut arr_i) elem_i x) ())
    | Length arr_i ->
        Res (result int exn, protect (fun () -> Dynarray.length (nth sut arr_i)) ())
    | Is_empty arr_i ->
        Res (result bool exn, protect (fun () -> Dynarray.is_empty (nth sut arr_i)) ())
    | Get_last arr_i ->
        Res (result elem exn, protect (fun () -> Dynarray.get_last (nth sut arr_i)) ())
    | Find_last arr_i ->
        Res (result (option elem) exn, protect (fun () -> Dynarray.find_last (nth sut arr_i)) ())
    | Copy arr_i ->
        Res (result unit exn, protect (fun () -> add_array (Dynarray.copy (nth sut arr_i)) sut) ())
    | Add_last (arr_i, x) ->
        Res (result unit exn, protect (fun () -> Dynarray.add_last (nth sut arr_i) x) ())
    | Append_array (arr_i, arr) ->
        Res (result unit exn, protect (fun () -> Dynarray.append_array (nth sut arr_i) arr) ())
    | Append_list (arr_i, l) ->
        Res (result unit exn, protect (fun () -> Dynarray.append_list (nth sut arr_i) l) ())
    | Append (arr_i1, arr_i2) ->
        Res (result unit exn, protect (fun () -> Dynarray.append (nth sut arr_i1) (nth sut arr_i2)) ())
    | Append_seq (arr_i, arr) ->
        Res (result unit exn, protect (fun () -> Dynarray.append_seq (nth sut arr_i) (Array.to_seq arr)) ())
    | Append_iter (arr_i, arr) ->
        Res (result unit exn, protect (fun () -> Dynarray.append_iter (nth sut arr_i) Array.iter arr) ())
    | Pop_last_opt arr_i ->
        Res (result (option elem) exn, protect (fun () -> Dynarray.pop_last_opt (nth sut arr_i)) ())
    | Remove_last arr_i ->
        Res (result unit exn, protect (fun () -> Dynarray.remove_last (nth sut arr_i)) ())
    | Truncate (arr_i, len) ->
        Res (result unit exn, protect (fun () -> Dynarray.truncate (nth sut arr_i) len) ())
    | Clear arr_i ->
        Res (result unit exn, protect (fun () -> Dynarray.clear (nth sut arr_i)) ())
    | Iter i ->
        Res (result unit exn, protect (fun () -> Dynarray.iter (fun x -> ignore @@ Sys.opaque_identity (ref x)) (nth sut i)) ())
    | Iteri i ->
        Res (result unit exn, protect (fun () -> Dynarray.iteri (fun i x -> ignore @@ Sys.opaque_identity (i, x)) (nth sut i)) ())
    | Map i ->
        Res (result unit exn, protect (fun () -> add_array (Dynarray.map Elem.mapping_fun (nth sut i)) sut) ())
    | Mapi i ->
        Res (result unit exn, protect (fun () -> add_array (Dynarray.mapi Elem.mapping_fun_with_index (nth sut i)) sut) ())
    | Fold_left (init, i) ->
        Res (result elem exn, protect (fun () -> Dynarray.fold_left Elem.folding_fun init (nth sut i)) ())
    | Fold_right (i, init) ->
        Res (result elem exn, protect (fun () -> Dynarray.fold_right Elem.folding_fun (nth sut i) init) ())
    | Exists i ->
        Res (result bool exn, protect (fun () -> Dynarray.exists Elem.pred (nth sut i)) ())
    | For_all i ->
        Res (result bool exn, protect (fun () -> Dynarray.for_all Elem.pred (nth sut i)) ())
    | Filter i ->
        Res (result unit exn, protect (fun () -> add_array (Dynarray.filter Elem.pred (nth sut i)) sut) ())
    | Filter_map i ->
        Res (result unit exn, protect (fun () -> add_array (Dynarray.filter_map Elem.filter_mapping_fun (nth sut i)) sut) ())
    | Of_array arr -> Res (unit, add_array (Dynarray.of_array arr) sut)
    | To_array i ->
        Res (result (array elem) exn, protect (fun () -> Dynarray.to_array (nth sut i)) ())
    | Of_list l -> Res (unit, add_array (Dynarray.of_list l) sut)
    | To_list i ->
        Res (result (list elem) exn, protect (fun () -> Dynarray.to_list (nth sut i)) ())
    | Of_seq arr -> Res (unit, add_array (Dynarray.of_seq (Array.to_seq arr)) sut)
    | To_seq i ->
        (* Evaluate the sequence immediately and store it as a list, otherwise
           sequence is lazily produced and later mutating operations can cause
           exceptions that are hard to model, even in a sequential setting. *)
        Res (result (list elem) exn, protect (fun () -> Dynarray.to_seq (nth sut i) |> List.of_seq) ())
    | To_seq_reentrant i ->
        Res (result (list elem) exn, protect (fun () -> Dynarray.to_seq_reentrant (nth sut i) |> List.of_seq) ())
    | To_seq_rev i ->
        Res (result (list elem) exn, protect (fun () -> Dynarray.to_seq_rev (nth sut i) |> List.of_seq) ())
    | To_seq_rev_reentrant i ->
        Res (result (list elem) exn, protect (fun () -> Dynarray.to_seq_rev_reentrant (nth sut i) |> List.of_seq) ())
    | Capacity i ->
        Res (result int exn, protect (fun () -> Dynarray.capacity (nth sut i)) ())
    | Ensure_capacity (arr_i, cap) ->
        Res (result unit exn, protect (fun () -> Dynarray.ensure_capacity (nth sut arr_i) cap) ())
    | Ensure_extra_capacity (arr_i, extra_cap) ->
        Res (result unit exn, protect (fun () -> Dynarray.ensure_extra_capacity (nth sut arr_i) extra_cap) ())
    | Fit_capacity arr_i ->
        Res (result unit exn, protect (fun () -> Dynarray.fit_capacity (nth sut arr_i)) ())
    | Set_capacity (arr_i, cap) ->
        Res (result unit exn, protect (fun () -> Dynarray.set_capacity (nth sut arr_i) cap) ())
    | Reset arr_i ->
        Res (result unit exn, protect (fun () -> Dynarray.reset (nth sut arr_i)) ())

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
    [ List.init 1024 (Fun.const 0xcafe) ]
  let mapping_fun = (~-)
  let mapping_fun_with_index i x = i + x
  let folding_fun = (+)
  let pred x = Int.equal 0 x
  let filter_mapping_fun x = if Int.compare x 0 < 0 then Some (-x) else None
end

module Float : Elem = struct
  type t = float
  let arb = QCheck.float
  let pp = Format.pp_print_float
  let equal = Float.equal
  let show = snd STM.float
  let init_state = [ [ 1.; 2.; 3. ]; List.init 12 Float.of_int ]
  let mapping_fun = (~-.)
  let mapping_fun_with_index i x = Float.of_int i +. x
  let folding_fun = (+.)
  let pred x = Float.equal 0. x
  let filter_mapping_fun x = if Float.compare x 0. < 0 then Some (-.x) else None
end

module Test_sequential = struct
  module Int = STM_sequential.Make (Dynarray_spec (Int))
  module Float = STM_sequential.Make (Dynarray_spec (Float))
end

module Test_domain = struct
  module Int = STM_domain.Make (Dynarray_spec (Int))
  module Float = STM_domain.Make (Dynarray_spec (Float))
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
