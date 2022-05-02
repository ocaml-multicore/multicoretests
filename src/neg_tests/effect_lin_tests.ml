open Lin_tests_common

(** This is a driver of the negative tests over the Effect module *)

(* Q: What constitutes a Fiber-unsafe API?
   A: Tests that behave differently on each run certainly do.
   Hack: Let's **NOT** properly reset the system-under-test between every test run to trigger a failure.
   Warning: because of the failure to properly reproduce these counterexamples will shrink badly  *)
module RT_int'    = Lin.Make(struct include RConf_int      let sut = init () let init () = sut end)
module RT_int64'  = Lin.Make(struct include RConf_int64    let sut = init () let init () = sut end)
module CLT_int'   = Lin.Make(struct include CLConf (Int)   let sut = init () let init () = sut end)
module CLT_int64' = Lin.Make(struct include CLConf (Int64) let sut = init () let init () = sut end)

;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count = 1_000 in
   [RT_int.lin_test     `Effect ~count ~name:"ref int test";
    RT_int64.lin_test   `Effect ~count ~name:"ref int64 test";
    CLT_int.lin_test    `Effect ~count ~name:"CList int test";
    CLT_int64.lin_test  `Effect ~count ~name:"CList int64 test";
    RT_int'.lin_test    `Effect ~count ~name:"ref int test";
    RT_int64'.lin_test  `Effect ~count ~name:"ref int64 test";
    CLT_int'.lin_test   `Effect ~count ~name:"CList int test";
    CLT_int64'.lin_test `Effect ~count ~name:"CList int64 test"])
