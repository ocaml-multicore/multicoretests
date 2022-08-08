open Lin_tests_common_dsl

(** This is a driver of the negative tests over the Domain module *)

;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count = 10000 in
   [RT_int.neg_lin_test    `Domain ~count ~name:"ref int test";
    RT_int64.neg_lin_test  `Domain ~count ~name:"ref int64 test";
    CLT_int.neg_lin_test   `Domain ~count ~name:"CList int test";
    CLT_int64.neg_lin_test `Domain ~count ~name:"CList int64 test"])
