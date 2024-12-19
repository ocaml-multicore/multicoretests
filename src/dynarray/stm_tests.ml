open QCheck
open STM

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

  type state = elem list list

  let arb_cmd state : cmd QCheck.arbitrary =
    let open Gen in
    let mid_int = Gen.int_bound 11_000 in
    let arr_idx state = (fun i -> I i) <$> (int_bound (List.length state - 1)) in
    let elem = Elem.arb.gen in
    QCheck.make ~print:show_cmd
      (frequency
        [ 5, return Create;
          5, (fun l x -> Make (l, x)) <$> mid_int <*> elem;
          ( 100
          , (fun arr_idx elem_idx -> Get (arr_idx, elem_idx))
            <$> arr_idx state
            <*> small_nat );
          ( 100
          , (fun arr_idx elem_idx x -> Set (arr_idx, elem_idx, x))
            <$> arr_idx state
            <*> small_nat
            <*> elem );
          100, (fun i -> Is_empty i) <$> arr_idx state;
          100, (fun i -> Length i) <$> arr_idx state;
          100, (fun i -> Get_last i) <$> arr_idx state;
          100, (fun i -> Find_last i) <$> arr_idx state;
          5, (fun i -> Copy i) <$> arr_idx state;
          100, (fun arr_i x -> Add_last (arr_i, x)) <$> arr_idx state <*> elem;
          33, (fun arr_i arr -> Append_array (arr_i, arr))
               <$> arr_idx state
               <*> array elem;
          33, (fun arr_i l -> Append_list (arr_i, l))
               <$> arr_idx state
               <*> list elem;
          33, (fun arr_i1 arr_i2 -> Append (arr_i1, arr_i2))
               <$> arr_idx state
               <*> arr_idx state;
          33, (fun arr_i arr -> Append_seq (arr_i, arr))
               <$> arr_idx state
               <*> array elem;
          33, (fun arr_i arr -> Append_iter (arr_i, arr)) <$> arr_idx state <*> array elem;
          100, (fun arr_i -> Pop_last_opt arr_i) <$> arr_idx state;
          100, (fun arr_i -> Remove_last arr_i) <$> arr_idx state;
          100, (fun arr_i len -> Truncate (arr_i, len))
                <$> arr_idx state
                <*> nat;
          100, (fun arr_i -> Clear arr_i) <$> arr_idx state;
          5, (fun i -> Iter i) <$> arr_idx state;
          5, (fun i -> Iteri i) <$> arr_idx state;
          5, (fun i -> Map i) <$> arr_idx state;
          5, (fun i -> Mapi i) <$> arr_idx state;
          5, (fun init i -> Fold_left (init, i)) <$> elem <*> arr_idx state;
          5, (fun i init -> Fold_right (i, init)) <$> arr_idx state <*> elem;
          50, (fun i -> Exists i) <$> arr_idx state;
          50, (fun i -> For_all i) <$> arr_idx state;
          5, (fun i -> Filter i) <$> arr_idx state;
          5, (fun i -> Filter_map i) <$> arr_idx state;
          5, (fun arr -> Of_array arr) <$> array elem;
          10, (fun i -> To_array i) <$> arr_idx state;
          5, (fun l -> Of_list l) <$> list elem;
          10, (fun i -> To_list i) <$> arr_idx state;
          5, (fun arr -> Of_seq arr) <$> array elem;
          50, (fun i -> To_seq i) <$> arr_idx state;
          50, (fun i -> To_seq_reentrant i) <$> arr_idx state;
          50, (fun i -> To_seq_rev i) <$> arr_idx state;
          50, (fun i -> To_seq_rev_reentrant i) <$> arr_idx state;
          100, (fun i -> Capacity i) <$> arr_idx state;
          100, (fun i cap -> Ensure_capacity (i, cap))
                <$> arr_idx state
                <*> nat;
          100, (fun i extra_cap -> Ensure_extra_capacity (i, extra_cap))
                <$> arr_idx state
                <*> small_nat;
          100, (fun i -> Fit_capacity i) <$> arr_idx state;
          100, (fun arr_i cap -> Set_capacity (arr_i, cap))
                <$> arr_idx state
                <*> nat;
          33, (fun arr_i -> Reset arr_i) <$> arr_idx state;
        ])

  let run : cmd -> sut -> res =
    fun cmd sut ->
    let nth sut (I idx) = List.nth !sut idx in
    match cmd with
    | Create -> Res (unit, add_array (Dynarray.create ()) sut)
    | Make (l, x) -> Res (unit, add_array (Dynarray.make l x) sut)
    | Get (arr_i, elem_i) ->
        Res (result elem exn, protect (fun () -> Dynarray.get (nth sut arr_i) elem_i) ())
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

  let init_state = Elem.init_state

  module List = struct
    include List

    let[@tail_mod_cons] rec take n =
      function
      | [] -> []
      | _ :: _ when n <= 0 -> []
      | x :: xs -> x :: take (n - 1) xs
  end

  let get_model (I arr_i) state = List.nth state arr_i

  let update_model (I arr_i) f state =
    List.mapi (fun i arr -> if i = arr_i then f arr else arr) state

  let next_state : cmd -> state -> state =
    fun cmd state ->
    match cmd with
    | Create -> [] :: state
    | Make (l, x) -> List.init l (Fun.const x) :: state
    | Get _ -> state
    | Set (arr_i, elem_i, x) ->
        update_model
          arr_i
          (fun arr -> List.mapi (fun i y -> if i = elem_i then x else y) arr)
          state
    | Length _
    | Is_empty _
    | Get_last _
    | Find_last _
    | To_array _
    | To_list _
    | To_seq _ -> state
    | Copy arr_i ->
        get_model arr_i state :: state
    | Add_last (arr_i, x) ->
        update_model arr_i (fun arr -> arr @ [ x ]) state
    | Append_array (arr_i, arr') ->
        update_model arr_i (fun arr -> arr @ Array.to_list arr') state
    | Append_list (arr_i, l) ->
        update_model arr_i (fun arr -> arr @ l) state
    | Append (arr_i1, arr_i2) ->
      (* "Warning: append a a is a programming error because it iterates on a and
          adds elements to it at the same time [...] It fails with Invalid_argument." *)
        update_model arr_i1 (fun arr -> arr @ get_model arr_i2 state) state
      (* In practice:
         Invalid_argument "Dynarray.append: a length change from 3 to 6 occurred during iteration"
         and the state change happens *)
    | Append_seq (arr_i, arr') ->
        update_model arr_i (fun arr -> arr @ Array.to_list arr') state
    | Append_iter (arr_i, arr') ->
        update_model arr_i (fun arr -> arr @ Array.to_list arr') state
    | Pop_last_opt arr_i ->
        update_model arr_i (fun arr -> List.take (List.length arr - 1) arr) state
    | Remove_last arr_i ->
        update_model
          arr_i
          (fun arr -> List.take (List.length arr - 1) arr)
          state
    | Truncate (arr_i, len) ->
        update_model arr_i (List.take len) state
    | Clear arr_i ->
        update_model arr_i (Fun.const []) state
    | Iter _
    | Iteri _ -> state
    | Map i -> List.map Elem.mapping_fun (get_model i state) :: state
    | Mapi i -> List.mapi Elem.mapping_fun_with_index (get_model i state) :: state
    | Fold_left _
    | Fold_right _
    | Exists _
    | For_all _ -> state
    | Filter i -> List.filter Elem.pred (get_model i state) :: state
    | Filter_map i ->
        List.filter_map Elem.filter_mapping_fun (get_model i state) :: state
    | Of_array arr -> Array.to_list arr :: state
    | Of_list l -> l :: state
    | Of_seq arr -> Array.to_list arr :: state
    | To_seq_reentrant _
    | To_seq_rev _
    | To_seq_rev_reentrant _
    | Capacity _ -> state
    | Ensure_capacity _
    | Ensure_extra_capacity _
    | Fit_capacity _ -> state
    | Set_capacity (arr_i, cap) -> update_model arr_i (fun arr -> List.take cap arr) state
    | Reset arr_i -> update_model arr_i (Fun.const []) state

  let valid_arr_idx (I idx) state = idx < List.length state

  let precond cmd state = match cmd with
    | Create
    | Make (_,_) -> true
    | Get (idx,_)
    | Set (idx,_,_)
    | Length idx
    | Is_empty idx
    | Get_last idx
    | Find_last idx
    | Copy idx
    | Add_last (idx,_)
    | Append_array (idx, _)
    | Append_list (idx, _) -> valid_arr_idx idx state
    | Append (idx, idx2) -> valid_arr_idx idx state && valid_arr_idx idx2 state
    | Append_seq (idx, _)
    | Append_iter (idx, _)
    | Pop_last_opt idx
    | Remove_last idx
    | Truncate (idx, _)
    | Clear idx
    | Iter idx
    | Iteri idx
    | Map idx
    | Mapi idx
    | Fold_left (_,idx)
    | Fold_right (idx,_)
    | Exists idx
    | For_all idx
    | Filter idx
    | Filter_map idx -> valid_arr_idx idx state
    | Of_array _ -> true
    | To_array idx -> valid_arr_idx idx state
    | Of_list _ -> true
    | To_list idx -> valid_arr_idx idx state
    | Of_seq _ -> true
    | To_seq idx
    | To_seq_reentrant idx
    | To_seq_rev idx
    | To_seq_rev_reentrant idx
    | Capacity idx
    | Ensure_capacity (idx, _)
    | Ensure_extra_capacity (idx, _)
    | Fit_capacity idx
    | Set_capacity (idx, _)
    | Reset idx -> valid_arr_idx idx state

  let postcond : cmd -> state -> res -> bool =
    fun cmd state res ->
    match cmd, res with
    | Create, _
    | Make _, _ -> true
    | Copy i, Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Add_last (i,_), Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Append_array (i,_), Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Append_list (i,_), Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Append (i,j), Res ((Result (Unit, Exn), _), res) ->
      valid_arr_idx i state &&  valid_arr_idx j state &&
      (match res with
       | Ok () -> true
       | Error (Invalid_argument _) -> equal_idx i j
       | Error _ -> false)
    | Append_seq (i,_), Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Append_iter (i,_), Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Remove_last i, Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Truncate (i,_), Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Clear i, Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Iter i, Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Iteri i, Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Map i, Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Mapi i, Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Filter i, Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Filter_map i, Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Of_array _, _
    | Of_list _, _
    | Of_seq _, _ -> true
    | Ensure_capacity (i,_), Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Ensure_extra_capacity (i,_), Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Fit_capacity i, Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Set_capacity (i,_), Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Reset i, Res ((Result (Unit, Exn), _), res) -> valid_arr_idx i state && res = Ok ()
    | Get (arr_i, elem_i), Res ((Result (Elem, Exn), _), res) ->
      (match valid_arr_idx arr_i state, res with
       | true, Ok r ->
          let arr = get_model arr_i state in
          let mres = List.nth arr elem_i in
          Elem.equal r mres
       | true, Error (Invalid_argument _) ->
          elem_i < 0 || elem_i >= List.length (get_model arr_i state)
       | false, Error (Failure msg) -> msg = "nth"
       | _,_ -> false)
    | Set (arr_i, elem_i, _), Res ((Result (Unit, Exn), _), res) ->
        valid_arr_idx arr_i state
        && (
          let arr = get_model arr_i state in
          (match res with
           | Ok () -> 0 <= elem_i && elem_i < List.length arr
           | Error (Invalid_argument _) -> elem_i < 0 || elem_i >= List.length arr
           | Error _ -> false)
        )
    | Length arr_i, Res ((Result (Int, Exn), _), res) ->
        valid_arr_idx arr_i state
        && (match res with
            | Ok l -> l = List.length (get_model arr_i state)
            | Error _ -> false)
    | Is_empty idx, Res ((Result (Bool, Exn), _), res) ->
        valid_arr_idx idx state
        && (match res with
            | Ok res -> Bool.equal res (List.is_empty (get_model idx state))
            | Error _ -> false)
    | Get_last idx, Res ((Result (Elem, Exn), _), res) ->
        valid_arr_idx idx state
        && (let arr = get_model idx state in
            match List.length arr, res with
            | 0, Error (Invalid_argument _) -> true
            | length, Ok res  ->
              length > 0 && Elem.equal res (List.nth arr (length - 1))
            | _, Error _ -> false (* Unexpected exception type *))
    | (Pop_last_opt idx | Find_last idx), Res ((Result (Option Elem, Exn), _), res) ->
        valid_arr_idx idx state
        && (let arr = get_model idx state in
            match List.length arr, res with
            | 0, Ok None -> true
            | length, Ok (Some res) when length > 0 ->
                Elem.equal res (List.nth arr (length - 1))
            | 0, Ok (Some _) (* unexpected [Some _] *)
            | _, Ok None     (* unexpected [None] *)
            | _, Error _ -> false
            | _, _ -> assert false (* length < 0: impossible *))
    | Fold_left (init, i), Res ((Result (Elem, Exn),_), res) ->
        valid_arr_idx i state
        && (match res with
            | Ok res -> Elem.equal res (List.fold_left Elem.folding_fun init (get_model i state))
            | Error _ -> false)
    | Fold_right (i, init), Res ((Result (Elem, Exn),_), res) ->
        valid_arr_idx i state
        && (match res with
            | Ok res -> Elem.equal res (List.fold_right Elem.folding_fun (get_model i state) init)
            | Error _ -> false)
    | Exists i, Res ((Result (Bool, Exn), _), res) ->
        valid_arr_idx i state
        && (match res with
            | Ok res -> Bool.equal res (List.exists Elem.pred (get_model i state))
            | Error _ -> false)
    | For_all i, Res ((Result (Bool, Exn), _), res) ->
        valid_arr_idx i state
        && (match res with
            | Ok res -> Bool.equal res (List.for_all Elem.pred (get_model i state))
            | Error _ -> false)
    | To_array i, Res ((Result (Array Elem, Exn), _), res) ->
        valid_arr_idx i state
        && (match res with
            | Ok arr ->
              let arr' = get_model i state in
              (try Array.for_all2 Elem.equal arr (Array.of_list arr')
               with Invalid_argument _ -> false)
            | Error _ -> false)
    | To_list i, Res ((Result (List Elem, Exn), _), res) ->
        valid_arr_idx i state
        && (match res with
            | Ok l ->
              let arr = get_model i state in
              (try List.for_all2 Elem.equal arr l
               with Invalid_argument _ -> false)
            | Error _ -> false)
    | To_seq i, Res ((Result (List Elem, Exn), _), res) ->
        valid_arr_idx i state
        && (match res with
            | Ok seq ->
              let arr = get_model i state in
              List.for_all2 Elem.equal seq arr
            | Error _ -> false)
    | To_seq_reentrant i, Res ((Result (List Elem, Exn), _), res) ->
        valid_arr_idx i state
        && (match res with
            | Ok seq ->
              let arr = get_model i state in
              List.for_all2 Elem.equal seq arr
              | Error _ -> false)
    | To_seq_rev i, Res ((Result (List Elem, Exn), _), res) ->
        valid_arr_idx i state
        && (match res with
            | Ok seq ->
              let arr = get_model i state in
              List.for_all2 Elem.equal seq (List.rev arr)
            | Error _ -> false)
    | To_seq_rev_reentrant i, Res ((Result (List Elem, Exn), _), res) ->
        valid_arr_idx i state
        && (match res with
            | Ok seq ->
              let arr = get_model i state in
              List.for_all2 Elem.equal seq (List.rev arr)
            | Error _ -> false)
    | Capacity i, Res ((Result (Int, Exn), _), res) ->
        (* The model here does not contain an actual notion of capacity, so
           only check that the result is greater than the actual length. *)
        valid_arr_idx i state
        && (match res with
            | Ok cap -> cap >= List.length (get_model i state)
            | Error _ -> false)
    | _ -> assert false
end

module Int : Elem = struct
  type t = int
  let arb = QCheck.int
  let pp = Format.pp_print_int
  let equal = Int.equal
  let show = snd STM.int
  let init_state = [ [ 1; 2; 3 ]; List.init 42 Fun.id ]
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
  let init_state = [ [ 1.; 2.; 3. ]; List.init 42 Float.of_int ]
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
    [ Test_sequential.Int.agree_test    ~count:1_000 ~name:"STM Dynarray test sequential agreement (int)";
      Test_domain.Int.stress_test_par   ~count:1_000 ~name:"STM Dynarray stress test (int)";
      Test_sequential.Float.agree_test  ~count:1_000 ~name:"STM Dynarray test sequential agreement (float)";
      Test_domain.Float.stress_test_par ~count:1_000 ~name:"STM Dynarray stress test (float)";
    ]
