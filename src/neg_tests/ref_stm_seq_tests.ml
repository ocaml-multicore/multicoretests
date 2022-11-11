open Ref_stm_spec

module RT_int_seq   = STM_sequential.Make(RConf_int)
module RT_int64_seq = STM_sequential.Make(RConf_int64)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count = 1000 in
   [RT_int_seq.agree_test   ~count ~name:"STM int ref test sequential";
    RT_int64_seq.agree_test ~count ~name:"STM int64 ref test sequential";
   ])
