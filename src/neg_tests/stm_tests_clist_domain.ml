open Stm_tests_clist_spec
module CLT_int_dom = STM_domain.Make(CLConf(Int))
module CLT_int64_dom = STM_domain.Make(CLConf(Int64))
;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [CLT_int_dom.neg_agree_test_par   ~count ~name:"STM int CList test parallel";
    CLT_int64_dom.neg_agree_test_par ~count ~name:"STM int64 CList test parallel"])
