open Lin

(** ********************************************************************** *)
(**                       Tests of a simple reference                      *)
(** ********************************************************************** *)

module Sut_int =
  struct
    let init () = ref 0
    let get r = !r
    let set r i = r:=i
    let add r i = let old = !r in r:= i + old (* buggy: not atomic *)
    let incr r = add r 1                      (* buggy: not atomic *)
    let decr r = add r (-1)                   (* buggy: not atomic *)
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

module Ref_int_spec : Spec = struct
  type t = int ref
  let init () = Sut_int.init ()
  let cleanup _ = ()
  let int = nat_small
  let api =
    [ val_ "Sut_int.get"  Sut_int.get  (t @-> returning int);
      val_ "Sut_int.set"  Sut_int.set  (t @-> int @-> returning unit);
      val_ "Sut_int.add"  Sut_int.add  (t @-> int @-> returning unit);
      val_ "Sut_int.incr" Sut_int.incr (t @-> returning unit);
      val_ "Sut_int.decr" Sut_int.decr (t @-> returning unit);
    ]
  end

module Ref_int64_spec : Spec = struct
  type t = int64 ref
  let init () = Sut_int64.init ()
  let cleanup _ = ()
  let int64 = nat64_small
  let api =
    [ val_ "Sut_int64.get"  Sut_int64.get  (t @-> returning int64);
      val_ "Sut_int64.set"  Sut_int64.set  (t @-> int64 @-> returning unit);
      val_ "Sut_int64.add"  Sut_int64.add  (t @-> int64 @-> returning unit);
      val_ "Sut_int64.incr" Sut_int64.incr (t @-> returning unit);
      val_ "Sut_int64.decr" Sut_int64.decr (t @-> returning unit);
    ]
  end

(** ********************************************************************** *)
(**                  Tests of the buggy concurrent list CList              *)
(** ********************************************************************** *)

module CList_spec_int : Spec =
struct
  type t = int CList.conc_list Atomic.t
  let init () = CList.list_init 0
  let cleanup _ = ()
  let int = nat_small
  let api =
    [ val_ "CList.add_node" CList.add_node (t @-> int @-> returning bool);
      val_ "CList.member"   CList.member  (t @-> int @-> returning bool);
    ]
  end

module CList_spec_int64 : Spec =
struct
  type t = int64 CList.conc_list Atomic.t
  let init () = CList.list_init Int64.zero
  let cleanup _ = ()
  let int64 = nat64_small
  let api =
    [ val_ "CList.add_node" CList.add_node (t @-> int64 @-> returning bool);
      val_ "CList.member"   CList.member  (t @-> int64 @-> returning bool);
    ]
  end
