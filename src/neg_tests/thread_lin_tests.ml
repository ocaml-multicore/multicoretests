open Lin_tests_common

(** This is a driver of the negative tests over the Thread module *)

;;
Util.set_ci_printing ()
;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [RT_int.lin_test       `Thread ~count ~name:"Lin ref int test with Thread"; (* unboxed, hence no allocations to trigger context switch *)
    RT_int64.neg_lin_test `Thread ~count:15000 ~name:"Lin ref int64 test with Thread";
    CLT_int.lin_test      `Thread ~count ~name:"Lin CList int test with Thread"; (* unboxed, hence no allocations to trigger context switch *)
    CLT_int64.lin_test    `Thread ~count ~name:"Lin CList int64 test with Thread"]) (* not triggering context switch, unfortunately *)
