open Lin_tests_common

module RT_int_domain = Lin_domain.Make(Ref_int_spec)
module RT_int64_domain = Lin_domain.Make(Ref_int64_spec)
module CLT_int_domain = Lin_domain.Make(CList_spec_int)
module CLT_int64_domain = Lin_domain.Make(CList_spec_int64)

(** This is a driver of the negative tests over the Domain module *)

;;
QCheck_base_runner.run_tests_main
  (let count = 10000 in
   [RT_int_domain.neg_lin_test    ~count ~name:"Lin ref int test with Domain";
    RT_int64_domain.neg_lin_test  ~count ~name:"Lin ref int64 test with Domain";
    CLT_int_domain.neg_lin_test   ~count ~name:"Lin CList int test with Domain";
    CLT_int64_domain.neg_lin_test ~count ~name:"Lin CList int64 test with Domain"])
