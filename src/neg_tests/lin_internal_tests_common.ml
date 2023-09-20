open QCheck

(* ********************************************************************** *)
(*                       Tests of a simple reference                      *)
(* ********************************************************************** *)
module Sut_int =
  struct
    let init () = ref 0
    let get r = !r
    let set r i = r:=i
    let add r i = let old = !r in r:= i + old (* buggy: not atomic *)
    let incr r = incr r                       (* buggy: not atomic *)
    let decr r = decr r                       (* buggy: not atomic *)
end

module Sut_int64 =
  struct
    let init () = ref Int64.zero
    let get r = !r
    let set r i = r:=i
    let add r i = let old = !r in r:= Int64.add i old (* buggy: not atomic *)
    let incr r = add r Int64.one                      (* buggy: not atomic *)
    let decr r = add r Int64.minus_one                (* buggy: not atomic *)
end

module RConf_int = struct
  type t = int ref

  type cmd =
    | Get
    | Set of int
    | Add of int
    | Incr
    | Decr

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Get -> cst0 "Get" fmt
    | Set x -> cst1 pp_int "Set" par fmt x
    | Add x -> cst1 pp_int "Add" par fmt x
    | Incr -> cst0 "Incr" fmt
    | Decr -> cst0 "Decr" fmt

  let show_cmd = Util.Pp.to_show pp_cmd

  let gen_cmd =
    let open QCheck.Gen in
    let int = nat in
    oneof
      [
        pure Get;
        map (fun x -> Set x) int;
        map (fun x -> Add x) int;
        pure Incr;
        pure Decr;
      ]

  let shrink_cmd c = match c with
    | Get
    | Incr
    | Decr -> Iter.empty
    | Set i -> Iter.map (fun i -> Set i) (Shrink.int i)
    | Add i -> Iter.map (fun i -> Add i) (Shrink.int i)

  type res =
    | RGet of int
    | RSet
    | RAdd
    | RIncr
    | RDecr

  let pp_res par fmt x =
    let open Util.Pp in
    match x with
    | RGet x -> cst1 pp_int "RGet" par fmt x
    | RSet -> cst0 "RSet" fmt
    | RAdd -> cst0 "RAdd" fmt
    | RIncr -> cst0 "RIncr" fmt
    | RDecr -> cst0 "RDecr" fmt

  let show_res = Util.Pp.to_show pp_res

  let equal_res x y =
    let open Util.Equal in
    match (x, y) with
    | RGet x, RGet y -> equal_int x y
    | RSet, RSet -> true
    | RAdd, RAdd -> true
    | RIncr, RIncr -> true
    | RDecr, RDecr -> true
    | _, _ -> false

  let init () = Sut_int.init ()

  let run c r = match c with
    | Get   -> RGet (Sut_int.get r)
    | Set i -> (Sut_int.set r i; RSet)
    | Add i -> (Sut_int.add r i; RAdd)
    | Incr  -> (Sut_int.incr r; RIncr)
    | Decr  -> (Sut_int.decr r; RDecr)

  let cleanup _ = ()
end


module RConf_int64 = struct
  type t = int64 ref

  type cmd =
    | Get
    | Set of int64
    | Add of int64
    | Incr
    | Decr

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Get -> cst0 "Get" fmt
    | Set x -> cst1 pp_int64 "Set" par fmt x
    | Add x -> cst1 pp_int64 "Add" par fmt x
    | Incr -> cst0 "Incr" fmt
    | Decr -> cst0 "Decr" fmt

  let show_cmd = Util.Pp.to_show pp_cmd

  let gen_cmd =
    let open QCheck.Gen in
    let int64 = map Int64.of_int nat in
    oneof
      [
        pure Get;
        map (fun x -> Set x) int64;
        map (fun x -> Add x) int64;
        pure Incr;
        pure Decr;
      ]

  let shrink_cmd c = match c with
    | Get
    | Incr
    | Decr -> Iter.empty
    | Set i -> Iter.map (fun i -> Set i) (Shrink.int64 i)
    | Add i -> Iter.map (fun i -> Add i) (Shrink.int64 i)

  type res =
    | RGet of int64
    | RSet
    | RAdd
    | RIncr
    | RDecr

  let pp_res par fmt x =
    let open Util.Pp in
    match x with
    | RGet x -> cst1 pp_int64 "RGet" par fmt x
    | RSet -> cst0 "RSet" fmt
    | RAdd -> cst0 "RAdd" fmt
    | RIncr -> cst0 "RIncr" fmt
    | RDecr -> cst0 "RDecr" fmt

  let show_res = Util.Pp.to_show pp_res

  let equal_res x y =
    let open Util.Equal in
    match (x, y) with
    | RGet x, RGet y -> equal_int64 x y
    | RSet, RSet -> true
    | RAdd, RAdd -> true
    | RIncr, RIncr -> true
    | RDecr, RDecr -> true
    | _, _ -> false

  let init () = Sut_int64.init ()

  let run c r = match c with
    | Get   -> RGet (Sut_int64.get r)
    | Set i -> (Sut_int64.set r i; RSet)
    | Add i -> (Sut_int64.add r i; RAdd)
    | Incr  -> (Sut_int64.incr r; RIncr)
    | Decr  -> (Sut_int64.decr r; RDecr)

  let cleanup _ = ()
end


(* ********************************************************************** *)
(*                  Tests of the buggy concurrent list CList              *)
(* ********************************************************************** *)
module CLConf (T : sig
                     type t
                     val zero : t
                     val of_int : int -> t
                     val to_string : t -> string
                     val shrink : t Shrink.t
                   end) =
struct
  module Lin = Lin.Internal [@alert "-internal"]

  type t = T.t CList.conc_list Atomic.t
  type int' = T.t

  type cmd =
    | Add_node of int'
    | Member of int'

  let pp_cmd par fmt x =
    let open Util.Pp in
    let pp_int' = of_show T.to_string in
    match x with
    | Add_node x -> cst1 pp_int' "Add_node" par fmt x
    | Member x -> cst1 pp_int' "Member" par fmt x

  let show_cmd = Util.Pp.to_show pp_cmd

  let gen_cmd =
    let open QCheck.Gen in
    let int' = map T.of_int nat in
    oneof
      [ map (fun x -> Add_node x) int'; map (fun x -> Member x) int' ]

  let shrink_cmd c = match c with
    | Add_node i -> Iter.map (fun i -> Add_node i) (T.shrink i)
    | Member i -> Iter.map (fun i -> Member i) (T.shrink i)

  type res =
    | RAdd_node of bool
    | RMember of bool

  let pp_res par fmt x =
    let open Util.Pp in
    match x with
    | RAdd_node x -> cst1 pp_bool "RAdd_node" par fmt x
    | RMember x -> cst1 pp_bool "RMember" par fmt x

  let show_res = Util.Pp.to_show pp_res

  let equal_res x y =
    let open Util.Equal in
    match (x, y) with
    | RAdd_node x, RAdd_node y -> equal_bool x y
    | RMember x, RMember y -> equal_bool x y
    | _, _ -> false

  let init () = CList.list_init T.zero

  let run c r = match c with
    | Add_node i -> RAdd_node (CList.add_node r i)
    | Member i   -> RMember (CList.member r i)

  let cleanup _ = ()
end

module Int = struct
  include Stdlib.Int
  let of_int (i:int) : t = i
  let shrink = Shrink.int
end

module Int64 = struct
  include Stdlib.Int64
  let shrink = Shrink.int64
end
