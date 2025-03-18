open Lin_tests_common

(** This is a driver of the negative tests over the Thread module *)

module CList_bis = struct include CList let add_node = add_node_thread end
module RT_int_thread = Lin_thread.Make(Ref_int_spec)
module RT_int64_thread = Lin_thread.Make(Ref_int64_spec)
module CLT_int_thread = Lin_thread.Make(CList_spec_int(CList_bis))
module CLT_int64_thread = Lin_thread.Make(CList_spec_int64(CList_bis))

;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [RT_int_thread.lin_test        ~count ~name:"Lin ref int test with Thread"; (* unboxed, hence no allocations to trigger context switch *)
    RT_int64_thread.neg_lin_test  ~count:15000 ~name:"Lin ref int64 test with Thread";
    CLT_int_thread.neg_lin_test   ~count ~name:"Lin CList int test with Thread"; (* unboxed, hence no allocations to trigger context switch *)
    CLT_int64_thread.neg_lin_test ~count ~name:"Lin CList int64 test with Thread"]) (* not triggering context switch, unfortunately *)
