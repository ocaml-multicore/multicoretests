open Lin_tests_common

(** This is a driver of the negative CList tests over the Thread module *)

;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [CLT_int.lin_test      `Thread ~count ~name:"Lin CList int test with Thread"; (* unboxed, hence no allocations to trigger context switch *)
    CLT_int64.lin_test    `Thread ~count ~name:"Lin CList int64 test with Thread"]) (* not triggering context switch, unfortunately *)
