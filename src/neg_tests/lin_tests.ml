open QCheck

(** ********************************************************************** *)
(**                       Tests of a simple reference                      *)
(** ********************************************************************** *)
module Sut =
  struct
    let init () = ref Int64.zero
    let get r = !r
    let set r i = r:=i
    let add r i = let old = !r in r:= Int64.add i old (* buggy: not atomic *)
    let incr r = add r Int64.one                          (* buggy: not atomic *)
    let decr r = add r Int64.minus_one                    (* buggy: not atomic *)
end

module RConf = struct
  type t = int64 ref

  type cmd =
    | Get
    | Set of int'
    | Add of int'
    | Incr
    | Decr [@@deriving qcheck, show { with_path = false }]
  and int' = int64 [@gen Gen.ui64]

  type res = RGet of int64 | RSet | RAdd | RIncr | RDecr [@@deriving show { with_path = false }]

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
  and int' = int64 [@gen (fun st -> Gen.nat st |> Int64.of_int)]

  type res = RAdd_node of bool | RMember of bool [@@deriving show { with_path = false }]

  let init () = CList.list_init Int64.zero

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
  RT.lin_test    `Domain ~count:1000 ~name:"ref test with Domains";
  RT.lin_test    `Thread ~count:1000 ~name:"ref test with Threads";
  CLT.lin_test   `Domain ~count:1000 ~name:"CList test with Domains";
  CLT.lin_test   `Thread ~count:1000 ~name:"CList test with Threads";
]
