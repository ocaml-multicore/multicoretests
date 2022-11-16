open Lin_tests_common
open Util

(** This is a driver of the negative tests over the Effect module *)

(* Q: What constitutes a Fiber-unsafe API?
   A: Tests that behave differently when run with/without a fiber-based scheduler certainly do.
   The following raises the Yield effect inside the `run` command.
   This results in an `Unhandled` exception when running outside a fiber-based scheduler,
   such as when interpreting these sequentially. *)
module RConf_int' =
struct
  include RConf_int
  type res = RGet of int | RSet | RAdd of (unit,exn) result | RIncr | RDecr [@@deriving show { with_path = false }, eq]

  let run c r = match c with
    | Get   -> RGet (Sut_int.get r)
    | Set i -> (Sut_int.set r i; RSet)
    | Add i -> (try let tmp = Sut_int.get r in Lin_effect.yield (); Sut_int.set r (tmp+i); RAdd (Ok ()) with exn -> RAdd (Error exn))
    | Incr  -> (Sut_int.incr r; RIncr)
    | Decr  -> (Sut_int.decr r; RDecr)
end

module RT_int_effect = Lin_effect.Make_internal(RConf_int)
module RT_int'_effect = Lin_effect.Make_internal(RConf_int')

module RConf_int64' =
struct
  include RConf_int64
  type res = RGet of int64 | RSet | RAdd of (unit,exn) result | RIncr | RDecr [@@deriving show { with_path = false }, eq]

  let run c r = match c with
    | Get   -> RGet (Sut_int64.get r)
    | Set i -> (Sut_int64.set r i; RSet)
    | Add i -> (try let tmp = Sut_int.get r in Lin_effect.yield (); Sut_int.set r (Int64.add tmp i); RAdd (Ok ()) with exn -> RAdd (Error exn))
    | Incr  -> (Sut_int64.incr r; RIncr)
    | Decr  -> (Sut_int64.decr r; RDecr)
end
module RT_int64_effect = Lin_effect.Make_internal(RConf_int64)
module RT_int64'_effect = Lin_effect.Make_internal(RConf_int64')

module CLConf_int' =
struct
  include CLConf(Int)
  type res = RAdd_node of (bool,exn) result | RMember of bool [@@deriving show { with_path = false }, eq]
  let run c r = match c with
    | Add_node i -> RAdd_node (try Lin_effect.yield (); Ok (CList.add_node r i) with exn -> Error exn)
    | Member i   -> RMember (CList.member r i)
end
module CLT_int_effect = Lin_effect.Make_internal(CLConf (Int))
module CLT_int'_effect = Lin_effect.Make_internal(CLConf_int')

module CLConf_int64' =
struct
  include CLConf(Int64)
  type res = RAdd_node of (bool,exn) result | RMember of bool [@@deriving show { with_path = false }, eq]
  let run c r = match c with
    | Add_node i -> RAdd_node (try Lin_effect.yield (); Ok (CList.add_node r i) with exn -> Error exn)
    | Member i   -> RMember (CList.member r i)
end
module CLT_int64_effect = Lin_effect.Make_internal(CLConf(Int64))
module CLT_int64'_effect = Lin_effect.Make_internal(CLConf_int64')
;;
QCheck_base_runner.run_tests_main
  (let count = 20_000 in [
      (* We don't expect the first four tests to fail as each `cmd` is completed before a `Yield` *)
      RT_int_effect.lin_test     ~count ~name:"Lin ref int test with Effect";
      RT_int64_effect.lin_test   ~count ~name:"Lin ref int64 test with Effect";
      CLT_int_effect.lin_test    ~count ~name:"Lin CList int test with Effect";
      CLT_int64_effect.lin_test  ~count ~name:"Lin CList int64 test with Effect";
      (* These next four tests are negative - and are expected to fail with exception `Unhandled` *)
      RT_int'_effect.neg_lin_test    ~count ~name:"negative Lin ref int test with Effect";
      RT_int64'_effect.neg_lin_test  ~count ~name:"negative Lin ref int64 test with Effect";
      CLT_int'_effect.neg_lin_test   ~count ~name:"negative Lin CList int test with Effect";
      CLT_int64'_effect.neg_lin_test ~count ~name:"negative Lin CList int64 test with Effect"
    ])
