open Stm_tests_spec_ref

module RT_int   = STM_thread.Make(RConf_int)   [@alert "-experimental"]
module RT_int64 = STM_thread.Make(RConf_int64) [@alert "-experimental"]
;;
if Sys.backend_type = Sys.Bytecode
then
  Printf.printf "STM ref tests with Thread disabled under bytecode\n\n%!"
else
QCheck_base_runner.run_tests_main
  [RT_int.agree_test_conc       ~count:250  ~name:"STM int ref test with Thread";
   RT_int64.neg_agree_test_conc ~count:5000 ~name:"STM int64 ref test with Thread";
  ]
