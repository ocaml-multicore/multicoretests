open Lin_tests_dsl_common

(** This is a driver of the negative tests over the Domain module *)

;;
QCheck_base_runner.run_tests_main
  (let count = 10000 in
   [RT_int.neg_lin_test    `Domain ~count ~name:"Lin_api ref int test with Domain";
    RT_int64.neg_lin_test  `Domain ~count ~name:"Lin_api ref int64 test with Domain";
    CLT_int.neg_lin_test   `Domain ~count ~name:"Lin_api CList int test with Domain";
    CLT_int64.neg_lin_test `Domain ~count ~name:"Lin_api CList int64 test with Domain"])
