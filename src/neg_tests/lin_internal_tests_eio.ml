open Lin_internal_tests_common

module RT_int_domain = Lin_domain_eio.Make_internal(RConf_int) [@alert "-internal"]
module RT_int64_domain = Lin_domain_eio.Make_internal(RConf_int64) [@alert "-internal"]
module CLT_int_domain = Lin_domain_eio.Make_internal(CLConf(CList)(Int)) [@alert "-internal"]
module CLT_int64_domain = Lin_domain_eio.Make_internal(CLConf(CList)(Int64)) [@alert "-internal"]

let () =
  Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let count = 15000 in
  QCheck_base_runner.run_tests_main
    [
      RT_int_domain.neg_lin_test ~domain_mgr ~count
        ~name:"Lin.Internal ref int test with Eio";
      RT_int64_domain.neg_lin_test ~domain_mgr ~count
        ~name:"Lin.Internal ref int64 test with Eio";
      CLT_int_domain.neg_lin_test ~domain_mgr ~count
        ~name:"Lin.Internal CList int test with Eio";
      CLT_int64_domain.neg_lin_test ~domain_mgr ~count
        ~name:"Lin.Internal CList int64 test with Eio";
    ]
