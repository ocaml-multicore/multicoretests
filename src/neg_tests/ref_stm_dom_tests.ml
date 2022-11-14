open Ref_stm_spec

module RT_int   = STM_domain.Make(RConf_int)
module RT_int64 = STM_domain.Make(RConf_int64)
;;
QCheck_runner.run_tests_main
  (let count = 1000 in
   [RT_int.neg_agree_test_par      ~count ~name:"STM int ref test parallel";
    RT_int64.neg_agree_test_par    ~count ~name:"STM int64 ref test parallel";
   ])
