open Stm_tests_clist_spec
module CList_bis = struct include CList let add_node = add_node_thread end
module CLT_int_thread = STM_thread.Make(CLConf(CList_bis)(Int))
module CLT_int64_thread = STM_thread.Make(CLConf(CList_bis)(Int64))
;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [CLT_int_thread.neg_agree_test_conc   ~count ~name:"STM int CList test with Thread";
    CLT_int64_thread.neg_agree_test_conc ~count ~name:"STM int64 CList test with Thread"])
