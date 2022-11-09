open Ref_stm_spec

module RT_int   = STM_thread.Make(RConf_int)
module RT_int64 = STM_thread.Make(RConf_int64)

module RConf_int_GC   = STM_base.AddGC(RConf_int)
module RConf_int64_GC = STM_base.AddGC(RConf_int64)

module RT_int_GC   = STM_thread.Make(RConf_int_GC)
module RT_int64_GC = STM_thread.Make(RConf_int64_GC)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count = 1000 in
   [ RT_int.agree_test_conc       ~count ~name:"STM int ref test with Thread";
     RT_int_GC.agree_test_conc    ~count ~name:"STM int ref test with Thread (w/AddGC functor)";
     RT_int64.neg_agree_test_conc ~count ~name:"STM int64 ref test with Thread";
     RT_int64_GC.agree_test_conc  ~count ~name:"STM int64 ref test with Thread (w/AddGC functor)";
   ])
