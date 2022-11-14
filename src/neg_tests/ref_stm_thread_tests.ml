open Ref_stm_spec

module RT_int   = STM_thread.Make(RConf_int)
module RT_int64 = STM_thread.Make(RConf_int64)
;;
QCheck_runner.run_tests_main
  (let count = 1000 in
   [RT_int.agree_test_conc       ~count ~name:"STM int ref test with Thread";
    RT_int64.neg_agree_test_conc ~count ~name:"STM int64 ref test with Thread";
   ])
