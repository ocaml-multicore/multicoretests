open Stm_tests_spec_ref

module RT_int   = STM_domain.Make(RConf_int)
module RT_int64 = STM_domain.Make(RConf_int64)

let rep_count = 25
let seq_len,par_len = 20,12
let retries = 10

let rt_int_neg_agree_test_par_asym ~count ~name =
  QCheck.Test.make_neg ~retries ~count ~name
    (RT_int.arb_triple_asym seq_len par_len RConf_int.arb_cmd RConf_int.arb_cmd RConf_int.arb_cmd)
    (Util.repeat rep_count RT_int.agree_prop_par_asym)

let rt_int64_neg_agree_test_par_asym ~count ~name =
  QCheck.Test.make_neg ~retries ~count ~name
    (RT_int64.arb_triple_asym seq_len par_len RConf_int64.arb_cmd RConf_int64.arb_cmd RConf_int64.arb_cmd)
    (Util.repeat rep_count RT_int64.agree_prop_par_asym)
;;
QCheck_runner.run_tests_main
  (let count = 1000 in
   [RT_int.neg_agree_test_par      ~count ~name:"STM int ref test parallel";
    RT_int64.neg_agree_test_par    ~count ~name:"STM int64 ref test parallel";
    rt_int_neg_agree_test_par_asym   ~count ~name:"STM int ref test parallel asymmetric";
    rt_int64_neg_agree_test_par_asym ~count ~name:"STM int64 ref test parallel asymmetric";
   ])
