open Lin_tests_common_dsl

(** This is a driver of the negative tests over the Thread module *)

;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [RT_int.lin_test       `Thread ~count ~name:"Lin_api ref int test with Thread";
    RT_int64.neg_lin_test `Thread ~count ~name:"Lin_api ref int64 test with Thread";
    CLT_int.lin_test      `Thread ~count ~name:"Lin_api CList int test with Thread";
    CLT_int64.lin_test    `Thread ~count ~name:"Lin_api CList int64 test with Thread"])
