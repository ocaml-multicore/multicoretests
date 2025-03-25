open Stm_tests_clist_spec
module CLT_int_seq = STM_sequential.Make(CLConf(CList)(Int))
module CLT_int64_seq = STM_sequential.Make(CLConf(CList)(Int64))
;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [CLT_int_seq.agree_test   ~count ~name:"STM int CList test sequential";
    CLT_int64_seq.agree_test ~count ~name:"STM int64 CList test sequential"])
