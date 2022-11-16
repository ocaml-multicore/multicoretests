open QCheck

(** ********************************************************************** *)
(**                       Tests of a simple reference                      *)
(** ********************************************************************** *)
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
    | Set of int'
    | Add of int'
    | Incr
    | Decr [@@deriving qcheck, show { with_path = false }]
  and int' = int [@gen Gen.nat]

  let shrink_cmd c = match c with
    | Get
    | Incr
    | Decr -> Iter.empty
    | Set i -> Iter.map (fun i -> Set i) (Shrink.int i)
    | Add i -> Iter.map (fun i -> Add i) (Shrink.int i)

  type res = RGet of int | RSet | RAdd | RIncr | RDecr [@@deriving show { with_path = false }, eq]

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
    | Set of int'
    | Add of int'
    | Incr
    | Decr [@@deriving qcheck, show { with_path = false }]
  and int' = int64 [@gen Gen.(map Int64.of_int nat)]

  let shrink_cmd c = match c with
    | Get
    | Incr
    | Decr -> Iter.empty
    | Set i -> Iter.map (fun i -> Set i) (Shrink.int64 i)
    | Add i -> Iter.map (fun i -> Add i) (Shrink.int64 i)

  type res = RGet of int64 | RSet | RAdd | RIncr | RDecr [@@deriving show { with_path = false }, eq]

  let init () = Sut_int64.init ()

  let run c r = match c with
    | Get   -> RGet (Sut_int64.get r)
    | Set i -> (Sut_int64.set r i; RSet)
    | Add i -> (Sut_int64.add r i; RAdd)
    | Incr  -> (Sut_int64.incr r; RIncr)
    | Decr  -> (Sut_int64.decr r; RDecr)

  let cleanup _ = ()
end

module RT_int_domain = Lin_domain.Make_internal(RConf_int)
module RT_int64_domain = Lin_domain.Make_internal(RConf_int64)


(** ********************************************************************** *)
(**                  Tests of the buggy concurrent list CList              *)
(** ********************************************************************** *)
module CLConf (T : sig
                     type t
                     val zero : t
                     val of_int : int -> t
                     val to_string : t -> string
                     val shrink : t Shrink.t
                   end) =
struct
  module Lin = Lin_base.Lin_internal

  type t = T.t CList.conc_list Atomic.t
  type int' = T.t
  let gen_int' = Gen.(map T.of_int nat)
  let pp_int' fmt t = Format.fprintf fmt "%s" (T.to_string t)

  type cmd =
    | Add_node of int'
    | Member of int' [@@deriving qcheck, show { with_path = false }]

  let shrink_cmd c = match c with
    | Add_node i -> Iter.map (fun i -> Add_node i) (T.shrink i)
    | Member i -> Iter.map (fun i -> Member i) (T.shrink i)

  type res = RAdd_node of bool | RMember of bool [@@deriving show { with_path = false }, eq]

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
module CLT_int_domain = Lin_domain.Make_internal(CLConf (Int))
module CLT_int64_domain = Lin_domain.Make_internal(CLConf (Int64))
