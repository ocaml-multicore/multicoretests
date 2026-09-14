open Stm_tests_ref_spec

module RT_int   = STM_domain_eio.Make(RConf_int)
module RT_int64 = STM_domain_eio.Make(RConf_int64)

let () =
  Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  QCheck_base_runner.run_tests_main
    [
      RT_int.neg_agree_test_par ~domain_mgr ~count:1000
        ~name:"STM int ref test parallel";
      RT_int64.neg_agree_test_par ~domain_mgr ~count:1000
        ~name:"STM int64 ref test parallel";
    ]
