open Lin_tests_common_dsl

(** This is a driver of the negative tests over the Domain module *)

;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
   [RT_int.lin_test    `Domain ~count:10   ~name:"ref int test";
    RT_int64.lin_test  `Domain ~count:10   ~name:"ref int64 test";
    CLT_int.lin_test   `Domain ~count:1000 ~name:"CList int test";
    CLT_int64.lin_test `Domain ~count:1000 ~name:"CList int64 test"]
