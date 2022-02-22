open QCheck

(** ********************************************************************** *)
(**                       Tests of a simple reference                      *)
(** ********************************************************************** *)
module Sut =
  struct
    let init () = ref 0
    let get r = !r
    let set r i = r:=i
    let add r i = let old = !r in r:=i + old (* buggy: not atomic *)
    let incr r = incr r     (* buggy: not guaranteed to be atomic *)
    let decr r = decr r     (* buggy: not guaranteed to be atomic *)
end

module RConf = struct
  type t = int ref

  type cmd =
    | Get
    | Set of int'
    | Add of int'
    | Incr
    | Decr [@@deriving qcheck, show { with_path = false }]
  and int' = int [@gen Gen.nat]

  type res = RGet of int | RSet | RAdd | RIncr | RDecr [@@deriving show { with_path = false }]

  let init () = Sut.init ()

  let run c r = match c with
    | Get   -> RGet (Sut.get r)
    | Set i -> (Sut.set r i; RSet)
    | Add i -> (Sut.add r i; RAdd)
    | Incr  -> (Sut.incr r; RIncr)
    | Decr  -> (Sut.decr r; RDecr)

  let cleanup _ = ()
end

module RT = Lin.Make(RConf)


(** ********************************************************************** *)
(**                  Tests of the buggy concurrent list CList              *)
(** ********************************************************************** *)
module CLConf =
struct
  type t = CList.conc_list Atomic.t

  type cmd =
    | Add_node of int'
    | Member of int' [@@deriving qcheck, show { with_path = false }]
  and int' = int [@gen Gen.nat]

  type res = RAdd_node of bool | RMember of bool [@@deriving show { with_path = false }]

  let init () = CList.list_init 0

  let run c r = match c with
    | Add_node i -> RAdd_node (CList.add_node r i)
    | Member i   -> RMember (CList.member r i)

  let cleanup _ = ()
end

module CLT = Lin.Make(CLConf)

;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main [
  RT.lin_test     ~count:1000 ~name:"ref test";
  CLT.lin_test    ~count:1000 ~name:"CList test";
]
