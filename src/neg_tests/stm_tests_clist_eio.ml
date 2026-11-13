open Stm_tests_clist_spec
module CLT_int_dom = STM_domain_eio.Make (CLConf (CList) (Int))
module CLT_int64_dom = STM_domain_eio.Make (CLConf (CList) (Int64))

let () =
  Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let count = 1000 in
  QCheck_base_runner.run_tests_main
    [
      CLT_int_dom.neg_agree_test_par ~domain_mgr ~count
        ~name:"STM int CList test parallel";
      CLT_int64_dom.neg_agree_test_par ~domain_mgr ~count
        ~name:"STM int64 CList test parallel";
    ]
