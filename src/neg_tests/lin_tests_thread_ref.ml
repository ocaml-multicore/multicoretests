open Lin_tests_common

(** This is a driver of the negative ref tests over the Thread module *)

;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [RT_int.lin_test       `Thread ~count ~name:"Lin ref int test with Thread"; (* unboxed, hence no allocations to trigger context switch *)
    RT_int64.neg_lin_test `Thread ~count:15000 ~name:"Lin ref int64 test with Thread"])
