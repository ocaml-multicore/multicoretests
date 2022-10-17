open Lin_tests_common

(** This is a driver of the negative tests over the Domain module *)

;;
Util.set_ci_printing ()
;;
QCheck_base_runner.run_tests_main
  (let count = 15000 in
   [RT_int.neg_lin_test    `Domain ~count ~name:"Lin ref int test with Domain";
    RT_int64.neg_lin_test  `Domain ~count ~name:"Lin ref int64 test with Domain";
    CLT_int.neg_lin_test   `Domain ~count ~name:"Lin CList int test with Domain";
    CLT_int64.neg_lin_test `Domain ~count ~name:"Lin CList int64 test with Domain"])
