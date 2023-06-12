open QCheck
open STM

(** parallel STM tests of Ephemeron *)

(*
module Ephemeron.S =
  sig
    type key
    type 'a t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_opt : 'a t -> key -> 'a option
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats : 'a t -> Hashtbl.statistics
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_keys : 'a t -> key Seq.t
    val to_seq_values : 'a t -> 'a Seq.t
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
    val clean : 'a t -> unit
    remove all dead bindings. Done automatically during automatic resizing.
    val stats_alive : 'a t -> Hashtbl.statistics
    same as Hashtbl.SeededS.stats but only count the alive bindings
 *)

module EphemeronModel =
  struct
    module E = Ephemeron.K1.Make(struct
                   [@@@warning "-unused-value-declaration"]
                   (* support Int64.hash added in 5.1, without triggering an 'unused hash' error *)
                   external seeded_hash_param :
                     int -> int -> int -> 'a -> int = "caml_hash" [@@noalloc]
                   let hash x = seeded_hash_param 10 100 0 x
                   include Stdlib.Int64
                 end)

    type key = int64
    type data = int64
    type sut = data E.t

    type state = (key * data) list
    type cmd =
      | Clear
      | Add of key * data
      | Remove of key
      | Find of key
      | Find_opt of key
      | Find_all of key
      | Replace of key * data
      | Mem of key
      | Length
      | Clean

    let pp_cmd par fmt x =
      let open Util.Pp in
      let pp_key = pp_int64 in
      let pp_data = pp_int64 in
      match x with
      | Clear -> cst0 "Clear" fmt
      | Add (x, y) -> cst2 pp_key pp_data "Add" par fmt x y
      | Remove x -> cst1 pp_key "Remove" par fmt x
      | Find x -> cst1 pp_key "Find" par fmt x
      | Find_opt x -> cst1 pp_key "Find_opt" par fmt x
      | Find_all x -> cst1 pp_key "Find_all" par fmt x
      | Replace (x, y) -> cst2 pp_key pp_data "Replace" par fmt x y
      | Mem x -> cst1 pp_key "Mem" par fmt x
      | Length -> cst0 "Length" fmt
      | Clean -> cst0 "Clean" fmt

    let show_cmd = Util.Pp.to_show pp_cmd

    let init_sut () = Gc.minor (); E.create 42
    let cleanup _ = ()

    let arb_cmd s =
      let key =
        if s = []
        then Gen.(map Int64.of_int small_int)
        else Gen.(oneof [oneofl (List.map fst s); map Int64.of_int small_int]) in
      let data = Gen.(map Int64.of_int small_int) in
      QCheck.make ~print:show_cmd
        Gen.(frequency
           [ 1,return Clear;
             2,map2 (fun k v -> Add (k, v)) key data;
             2,map  (fun k -> Remove k) key;
             3,map  (fun k -> Find k) key;
             3,map  (fun k -> Find_opt k) key;
             3,map  (fun k -> Find_all k) key;
             2,map2 (fun k v -> Replace (k, v)) key data;
             3,map  (fun k -> Mem k) key;
             2,return Length;
             1,return Clean; ])

    let next_state c s =
      match c with
      | Clear -> []
      | Add (k, v) -> (k,v)::s
      | Remove k -> List.remove_assoc k s
      | Find _
      | Find_opt _
      | Find_all _ -> s
      | Replace (k,v) -> (k,v)::(List.remove_assoc k s)
      | Mem _
      | Length
      | Clean -> s

    let run c e =
      let data = int64 in
      match c with
      | Clear -> Res (unit, E.clear e)
      | Add (k, v) -> Res (unit, E.add e k v)
      | Remove k -> Res (unit, E.remove e k)
      | Find k -> Res (result data exn, protect (E.find e) k)
      | Find_opt k -> Res (option data, E.find_opt e k)
      | Find_all k -> Res (list data, E.find_all e k)
      | Replace (k,v) -> Res (unit, E.replace e k v)
      | Mem k -> Res (bool, E.mem e k)
      | Length -> Res (int, E.length e)
      | Clean -> Res (unit, E.clean e)

    let init_state = []

    let precond _ _ = true
    let postcond c (s : state) res =
      match c,res with
      | Clear, Res ((Unit,_),_) -> true
      | Add (_,_), Res ((Unit,_),_) -> true
      | Remove _, Res ((Unit,_),_) -> true
      | Find k,   Res ((Result (Int64,Exn),_),r) ->
          r = Error Not_found || r = protect (List.assoc k) s
      | Find_opt k, Res ((Option Int64,_),r) ->
          r = None || r = List.assoc_opt k s
      | Find_all k, Res ((List Int64,_),r) ->
         let filter = fun (k',v') -> if k' = k then Some v' else None in
         let vs_state = List.filter_map filter s in
         (* some entries may have been GC'ed - test only for inclusion *)
         List.for_all (fun v -> List.mem v vs_state) (List.sort Int64.compare r)
      | Replace (_,_), Res ((Unit,_),_) -> true
      | Mem k, Res ((Bool,_),r) -> r = false || r = List.mem_assoc k s (*effectively: no postcond*)
      | Length, Res ((Int,_),r) -> r <= List.length s
      | Clean, Res ((Unit,_),_) -> true
      | _ -> false
  end

module ETest_seq = STM_sequential.Make(EphemeronModel)
module ETest_dom = STM_domain.Make(EphemeronModel)

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
let count = 1000
let status_seq =
  run_tests
    [ ETest_seq.agree_test         ~count ~name:"STM Ephemeron test sequential"; ]
let () = Gc.full_major ()
let status_par =
  run_tests
    [ ETest_dom.neg_agree_test_par ~count ~name:"STM Ephemeron test parallel"; ]
let _ = exit (if status_seq=0 && status_par=0 then 0 else 1)
