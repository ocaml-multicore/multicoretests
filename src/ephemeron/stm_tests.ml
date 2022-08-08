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
                   type t = Char.t
                   let equal = Char.equal
                   let hash = Hashtbl.hash
                 end)

    type t = string E.t

    type sut = string E.t
    type state = (char * string) list
    type cmd =
      | Clear
      | Add of char * string
      | Remove of char
      | Find of char
      | Find_opt of char
      | Find_all of char
      | Replace of char * string
      | Mem of char
      | Length
      | Clean
    [@@deriving show { with_path = false } ]

    let init_sut () = E.create 42
    let cleanup _ = ()

    let arb_cmd s =
      let key =
        if s = []
        then Gen.printable
        else Gen.(oneof [oneofl (List.map fst s); printable]) in
      let value = Gen.small_string ~gen:Gen.printable in
      QCheck.make ~print:show_cmd
        Gen.(frequency
           [ 1,return Clear;
             3,map2 (fun k v -> Add (k, v)) key value;
             3,map  (fun k -> Remove k) key;
             3,map  (fun k -> Find k) key;
             3,map  (fun k -> Find_opt k) key;
             3,map  (fun k -> Find_all k) key;
             3,map2 (fun k v -> Replace (k, v)) key value;
             3,map  (fun k -> Mem k) key;
             3,return Length;
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
      match c with
      | Clear -> Res (unit, E.clear e)
      | Add (k, v) -> Res (unit, E.add e k v)
      | Remove k -> Res (unit, E.remove e k)
      | Find k -> Res (result string exn, protect (E.find e) k)
      | Find_opt k -> Res (option string, E.find_opt e k)
      | Find_all k -> Res (list string, E.find_all e k)
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
      | Find k,   Res ((Result (String,Exn),_),r) ->
          r = Error Not_found || r = protect (List.assoc k) s
      | Find_opt k, Res ((Option String,_),r) ->
          r = None || r = List.assoc_opt k s
      | Find_all k, Res ((List String,_),r) ->
         let filter = fun (k',v') -> if k' = k then Some v' else None in
         let vs_state = List.filter_map filter s in
         (* some entries may have been GC'ed - test only for inclusion *)
         List.for_all (fun v -> List.mem v vs_state) (List.sort String.compare r)
      | Replace (_,_), Res ((Unit,_),_) -> true
      | Mem k, Res ((Bool,_),r) -> r = false || r = List.mem_assoc k s (*effectively: no postcond*)
      | Length, Res ((Int,_),r) -> r <= List.length s
      | Clean, Res ((Unit,_),_) -> true
      | _ -> false
  end

module ETest = STM.Make(EphemeronModel)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count,name = 1000,"Ephemeron test" in
   [ ETest.agree_test         ~count ~name; (* succeed *)
     ETest.neg_agree_test_par ~count ~name; (* fail *)
  ])

