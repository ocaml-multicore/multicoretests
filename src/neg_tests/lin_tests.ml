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
module CLConf (T : sig type t val zero : t val of_int : int -> t val to_string : t -> string end) =
struct
  type t = T.t CList.conc_list Atomic.t
  type int' = T.t
  let gen_int' = Gen.(map T.of_int nat)
  let pp_int' fmt t = Format.fprintf fmt "%s" (T.to_string t)

  type cmd =
    | Add_node of int'
    | Member of int' [@@deriving qcheck, show { with_path = false }]

  type res = RAdd_node of bool | RMember of bool [@@deriving show { with_path = false }]

  let init () = CList.list_init T.zero

  let run c r = match c with
    | Add_node i -> RAdd_node (CList.add_node r i)
    | Member i   -> RMember (CList.member r i)

  let cleanup _ = ()
end

module Int = struct
  include Stdlib.Int
  let of_int (i:int) : t = i
end

module CLT_int = Lin.Make(CLConf (Int))
module CLT_int64 = Lin.Make(CLConf (Int64))

;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count = 1000 in
   [RT_int.lin_test    `Domain ~count ~name:"ref int test with Domains";
    RT_int.lin_test    `Thread ~count ~name:"ref int test with Threads";
    RT_int64.lin_test  `Domain ~count ~name:"ref int64 test with Domains";
    RT_int64.lin_test  `Thread ~count ~name:"ref int64 test with Threads";
    CLT_int.lin_test   `Domain ~count ~name:"CList int test with Domains";
    CLT_int.lin_test   `Thread ~count ~name:"CList int test with Threads";
    CLT_int64.lin_test `Domain ~count ~name:"CList test64 with Domains";
    CLT_int64.lin_test `Thread ~count ~name:"CList test64 with Threads"])
