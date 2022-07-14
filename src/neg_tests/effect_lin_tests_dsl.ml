open Lin_tests_common_dsl
open Lin_api

(** This is a driver of the negative tests over the Effect module *)

(* Q: What constitutes a Fiber-unsafe API?
   A: Tests that behave differently when run with/without a fiber-based scheduler certainly do.
   The following raises the Yield effect inside the `run` command.
   This results in an `Unhandled` exception when running outside a fiber-based scheduler,
   such as when interpreting these sequentially. *)
module Sut_int' = struct
  include Sut_int
  let add r i = let old = !r in Lin.yield (); set r (old+i)
end

module Ref_int'_spec : Lin_api.ApiSpec = struct
  type t = int ref
  let init () = Sut_int'.init ()
  let cleanup _ = ()
  let api =
    [ val_ "Sut_int'.get"  Sut_int'.get  (t @-> returning int);
      val_ "Sut_int'.set"  Sut_int'.set  (t @-> int @-> returning unit);
      val_ "Sut_int'.add"  Sut_int'.add  (t @-> int @-> returning_or_exc unit);
      val_ "Sut_int'.incr" Sut_int'.incr (t @-> returning unit);
      val_ "Sut_int'.decr" Sut_int'.decr (t @-> returning unit);
    ]
  end

module RT_int' = Lin_api.Make(Ref_int'_spec)

module Sut_int64' = struct
  include Sut_int64
  let add r i = let old = !r in Lin.yield (); set r (Int64.add old i)
end

module Ref_int64'_spec : Lin_api.ApiSpec = struct
  type t = int64 ref
  let init () = Sut_int64'.init ()
  let cleanup _ = ()
  let api =
    [ val_ "Sut_int64'.get"  Sut_int64'.get  (t @-> returning int64);
      val_ "Sut_int64'.set"  Sut_int64'.set  (t @-> int64 @-> returning unit);
      val_ "Sut_int64'.add"  Sut_int64'.add  (t @-> int64 @-> returning_or_exc unit);
      val_ "Sut_int64'.incr" Sut_int64'.incr (t @-> returning unit);
      val_ "Sut_int64'.decr" Sut_int64'.decr (t @-> returning unit);
    ]
  end

module RT_int64' = Lin_api.Make(Ref_int64'_spec)


module CList_spec_int' : Lin_api.ApiSpec =
struct
  type t = int CList.conc_list Atomic.t
  let init () = CList.list_init 0
  let cleanup _ = ()
  let add_node r i = Lin.yield (); CList.add_node r i
  let api =
    [ val_ "CList.add_node" add_node (t @-> int @-> returning_or_exc bool);
      val_ "CList.member"   CList.member  (t @-> int @-> returning bool);
    ]
  end
module CList_spec_int64' : Lin_api.ApiSpec =
struct
  type t = int64 CList.conc_list Atomic.t
  let init () = CList.list_init Int64.zero
  let add_node r i = Lin.yield (); CList.add_node r i
  let cleanup _ = ()
  let api =
    [ val_ "CList.add_node" add_node (t @-> int64 @-> returning_or_exc bool);
      val_ "CList.member"   CList.member  (t @-> int64 @-> returning bool);
    ]
  end

module CLT_int' = Lin_api.Make(CList_spec_int')
module CLT_int64' = Lin_api.Make(CList_spec_int64')

;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count = 20_000 in [
      (* We don't expect the first four tests to fail as each `cmd` is completed before a `Yield` *)
      RT_int.lin_test     `Effect ~count ~name:"ref int test";
      RT_int64.lin_test   `Effect ~count ~name:"ref int64 test";
      CLT_int.lin_test    `Effect ~count ~name:"CList int test";
      CLT_int64.lin_test  `Effect ~count ~name:"CList int64 test";
      (* These next four tests are negative - and are expected to fail with exception `Unhandled` *)
      RT_int'.neg_lin_test    `Effect ~count ~name:"negative ref int test";
      RT_int64'.neg_lin_test  `Effect ~count ~name:"negative ref int64 test";
      CLT_int'.neg_lin_test   `Effect ~count ~name:"negative CList int test";
      CLT_int64'.neg_lin_test `Effect ~count ~name:"negative CList int64 test"
    ])
