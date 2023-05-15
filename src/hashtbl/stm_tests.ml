open QCheck
open STM

(** parallel STM tests of Hashtbl *)

(*
module Hashtbl :
  sig
    type (!'a, !'b) t
    val create : ?random:bool -> int -> ('a, 'b) t
    val clear : ('a, 'b) t -> unit
    val reset : ('a, 'b) t -> unit
    val copy : ('a, 'b) t -> ('a, 'b) t
    val add : ('a, 'b) t -> 'a -> 'b -> unit
    val find : ('a, 'b) t -> 'a -> 'b
    val find_opt : ('a, 'b) t -> 'a -> 'b option
    val find_all : ('a, 'b) t -> 'a -> 'b list
    val mem : ('a, 'b) t -> 'a -> bool
    val remove : ('a, 'b) t -> 'a -> unit
    val replace : ('a, 'b) t -> 'a -> 'b -> unit
    val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
    val filter_map_inplace : ('a -> 'b -> 'b option) -> ('a, 'b) t -> unit
    val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
    val length : ('a, 'b) t -> int
    val randomize : unit -> unit
    val is_randomized : unit -> bool
    val rebuild : ?random:bool -> ('a, 'b) t -> ('a, 'b) t
    ...
end
*)


module HConf =
struct
  type sut = (char, int) Hashtbl.t
  type state = (char * int) list
  type cmd =
    | Clear
    | Add of char * int
    | Remove of char
    | Find of char
    | Find_opt of char
    | Find_all of char
    | Replace of char * int
    | Mem of char
    | Length

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Clear -> cst0 "Clear" fmt
    | Add (x, y) -> cst2 pp_char pp_int "Add" par fmt x y
    | Remove x -> cst1 pp_char "Remove" par fmt x
    | Find x -> cst1 pp_char "Find" par fmt x
    | Find_opt x -> cst1 pp_char "Find_opt" par fmt x
    | Find_all x -> cst1 pp_char "Find_all" par fmt x
    | Replace (x, y) -> cst2 pp_char pp_int "Replace" par fmt x y
    | Mem x -> cst1 pp_char "Mem" par fmt x
    | Length -> cst0 "Length" fmt

  let show_cmd = Util.Pp.to_show pp_cmd

  let init_sut () = Hashtbl.create ~random:false 42
  let cleanup _ = ()

  let arb_cmd s =
    let char =
      if s=[]
      then Gen.printable
      else Gen.(oneof [oneofl (List.map fst s); printable]) in
    let int = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [Gen.return Clear;
          Gen.map2 (fun k v -> Add (k,v)) char int;
          Gen.map  (fun k   -> Remove k) char;
          Gen.map  (fun k   -> Find k) char;
          Gen.map  (fun k   -> Find_opt k) char;
          Gen.map  (fun k   -> Find_all k) char;
          Gen.map2 (fun k v -> Replace (k,v)) char int;
          Gen.map  (fun k   -> Mem k) char;
          Gen.return Length;
         ])

  let next_state c s = match c with
    | Clear         -> []
    | Add (k,v)     -> (k,v)::s
    | Remove k      -> List.remove_assoc k s
    | Find _
    | Find_opt _
    | Find_all _    -> s
    | Replace (k,v) -> (k,v)::(List.remove_assoc k s)
    | Mem _
    | Length        -> s

  let run c h =
    match c with
    | Clear         -> Res (unit, Hashtbl.clear h)
    | Add (k,v)     -> Res (unit, Hashtbl.add h k v)
    | Remove k      -> Res (unit, Hashtbl.remove h k)
    | Find k        -> Res (result int exn, protect (Hashtbl.find h) k)
    | Find_opt k    -> Res (option int, Hashtbl.find_opt h k)
    | Find_all k    -> Res (list int, Hashtbl.find_all h k)
    | Replace (k,v) -> Res (unit, Hashtbl.replace h k v)
    | Mem k         -> Res (bool, Hashtbl.mem h k)
    | Length        -> Res (int, Hashtbl.length h)

  let init_state  = []

  let precond _ _ = true
  let postcond c (s : state) res =
    match c,res with
    | Clear,         Res ((Unit,_),_)
    | Add (_,_),     Res ((Unit,_),_)
    | Replace (_,_), Res ((Unit,_),_) -> true
    | Remove _,      Res ((Unit,_),_) -> true
    | Find k,        Res ((Result (Int,Exn),_),r) ->
        r = (try Ok (List.assoc k s)
             with Not_found -> Error Not_found)
    | Find_opt k,    Res ((Option Int,_),r) -> r = List.assoc_opt k s
    | Find_all k,    Res ((List Int,_),r) ->
        let rec find_all h = match h with
          | [] -> []
          | (k',v')::h' ->
              if k = k' (*&& k<>'a'*) (* an arbitrary, injected bug *)
              then v'::find_all h'
              else find_all h' in
        r = find_all s
    | Mem k,         Res ((Bool,_),r) -> r = List.mem_assoc k s
    | Length,        Res ((Int,_),r) -> r = List.length s
    | _ -> false
end


module HTest_seq = STM_sequential.Make(HConf)
module HTest_dom = STM_domain.Make(HConf)
;;
QCheck_base_runner.run_tests_main
  (let count = 200 in
   [HTest_seq.agree_test         ~count ~name:"STM Hashtbl test sequential";
    HTest_dom.neg_agree_test_par ~count ~name:"STM Hashtbl test parallel";
   ])
