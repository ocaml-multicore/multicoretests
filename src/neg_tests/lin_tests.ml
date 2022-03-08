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
    let incr r = incr r                     (* buggy: not atomic *)
    let decr r = decr r                (* buggy: not atomic *)
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

  type res = RGet of int | RSet | RAdd | RIncr | RDecr [@@deriving show { with_path = false }]

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
  and int' = int64 [@gen Gen.ui64]

  type res = RGet of int64 | RSet | RAdd | RIncr | RDecr [@@deriving show { with_path = false }]

  let init () = Sut_int64.init ()

  let run c r = match c with
    | Get   -> RGet (Sut_int64.get r)
    | Set i -> (Sut_int64.set r i; RSet)
    | Add i -> (Sut_int64.add r i; RAdd)
    | Incr  -> (Sut_int64.incr r; RIncr)
    | Decr  -> (Sut_int64.decr r; RDecr)

  let cleanup _ = ()
end

module RT_int = Lin.Make(RConf_int)
module RT_int64 = Lin.Make(RConf_int64)


(** ********************************************************************** *)
(**                  Tests of the buggy concurrent list CList              *)
(** ********************************************************************** *)
module CLConf (T : sig type t val dummy : t val f : int -> t val pp : t -> string end) =
struct
  module CL = CList.Make (struct type t = T.t end)

  type t = CL.conc_list Atomic.t
  let gen_int' st = Gen.nat st |> T.f
  type int' = T.t

  type cmd =
    | Add_node of int' [@printer fun fmt t -> fprintf fmt "Add_node %s" (T.pp t)]
    | Member of int' [@printer fun fmt t -> fprintf fmt "Member %s" (T.pp t)] [@@deriving qcheck, show { with_path = false }]

  type res = RAdd_node of bool | RMember of bool [@@deriving show { with_path = false }]

  let init () = CL.list_init T.dummy

  let run c r = match c with
    | Add_node i -> RAdd_node (CL.add_node r i)
    | Member i   -> RMember (CL.member r i)

  let cleanup _ = ()
end

module T_int = struct
  type t = int
  let dummy = 0
  let f i = i
  let pp = Int.to_string
end

module T_int64 = struct
  type t = int64
  let dummy = Int64.zero
  let f = Int64.of_int
  let pp = Int64.to_string
end

module CLT_int = Lin.Make(CLConf (T_int))
module CLT_int64 = Lin.Make(CLConf (T_int64))

;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main [
  RT_int.lin_test    `Domain ~count:1000 ~name:"ref int test with Domains";
  RT_int64.lin_test    `Domain ~count:1000 ~name:"ref int64 test with Domains";
  RT_int64.lin_test    `Thread ~count:1000 ~name:"ref int64 test with Threads";
  CLT_int.lin_test   `Domain ~count:1000 ~name:"CList int test with Domains";
  CLT_int.lin_test   `Thread ~count:1000 ~name:"CList int test with Threads";
  CLT_int64.lin_test   `Domain ~count:1000 ~name:"CList test64 with Domains";
  CLT_int64.lin_test   `Thread ~count:1000 ~name:"CList test64 with Threads";
]
