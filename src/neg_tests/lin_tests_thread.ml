open Lin_tests_common

(** This is a driver of the negative tests over the Thread module *)

module RT_int_thread = Lin_thread.Make(Ref_int_spec) [@alert "-experimental"]
module RT_int64_thread = Lin_thread.Make(Ref_int64_spec) [@alert "-experimental"]
module CLT_int_thread = Lin_thread.Make(CList_spec_int) [@alert "-experimental"]
module CLT_int64_thread = Lin_thread.Make(CList_spec_int64) [@alert "-experimental"]

;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [RT_int_thread.lin_test       ~count ~name:"Lin ref int test with Thread"; (* unboxed, hence no allocations to trigger context switch *)
    RT_int64_thread.neg_lin_test ~count:15000 ~name:"Lin ref int64 test with Thread";
    CLT_int_thread.lin_test      ~count ~name:"Lin CList int test with Thread"; (* unboxed, hence no allocations to trigger context switch *)
    CLT_int64_thread.lin_test    ~count ~name:"Lin CList int64 test with Thread"]) (* not triggering context switch, unfortunately *)
