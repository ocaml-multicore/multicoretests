open Lin_tests_common

(** This is a driver of the negative tests over the Thread module *)

;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count = 1000 in
   [RT_int.lin_test        `Thread ~count ~name:"ref int test";   (* unboxed, hence no allocations to trigger context switch *)
    RT_int64.neg_lin_test  `Thread ~count ~name:"ref int64 test";
    CLT_int.lin_test       `Thread ~count ~name:"CList int test"; (* unboxed, hence no allocations to trigger context switch *)
    CLT_int64.lin_test `Thread ~count ~name:"CList int64 test"])  (* not triggering context switch, unfortunately *)
