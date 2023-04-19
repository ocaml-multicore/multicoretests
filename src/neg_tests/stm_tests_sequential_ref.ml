open Stm_tests_spec_ref

module RT_int_seq   = STM_sequential.Make(RConf_int)
module RT_int64_seq = STM_sequential.Make(RConf_int64)
;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [RT_int_seq.agree_test   ~count ~name:"STM int ref test sequential";
    RT_int64_seq.agree_test ~count ~name:"STM int64 ref test sequential";
   ])
