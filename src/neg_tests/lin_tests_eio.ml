open Lin_tests_common

module RT_int_domain = Lin_domain_eio.Make(Ref_int_spec)
module RT_int64_domain = Lin_domain_eio.Make(Ref_int64_spec)
module CLT_int_domain = Lin_domain_eio.Make(CList_spec_int(CList))
module CLT_int64_domain = Lin_domain_eio.Make(CList_spec_int64(CList))

let () =
  Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let count = 10000 in
  QCheck_base_runner.run_tests_main
    [
      RT_int_domain.neg_lin_test ~domain_mgr ~count
        ~name:"Lin ref int test with Eio";
      RT_int64_domain.neg_lin_test ~domain_mgr ~count
        ~name:"Lin ref int64 test with Eio";
      CLT_int_domain.neg_lin_test ~domain_mgr ~count
        ~name:"Lin CList int test with Eio";
      CLT_int64_domain.neg_lin_test ~domain_mgr ~count
        ~name:"Lin CList int64 test with Eio";
    ]
