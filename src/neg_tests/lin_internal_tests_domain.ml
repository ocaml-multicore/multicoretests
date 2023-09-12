open Lin_internal_tests_common

module RT_int_domain = Lin_domain.Make_internal(RConf_int) [@alert "-internal"]
module RT_int64_domain = Lin_domain.Make_internal(RConf_int64) [@alert "-internal"]
module CLT_int_domain = Lin_domain.Make_internal(CLConf (Int)) [@alert "-internal"]
module CLT_int64_domain = Lin_domain.Make_internal(CLConf (Int64)) [@alert "-internal"]

(** This is a driver of the negative tests over the Domain module *)

;;
QCheck_base_runner.run_tests_main
  (let count = 15000 in
   [RT_int_domain.neg_lin_test    ~count ~name:"Lin.Internal ref int test with Domain";
    RT_int64_domain.neg_lin_test  ~count ~name:"Lin.Internal ref int64 test with Domain";
    CLT_int_domain.neg_lin_test   ~count ~name:"Lin.Internal CList int test with Domain";
    CLT_int64_domain.neg_lin_test ~count ~name:"Lin.Internal CList int64 test with Domain"])
