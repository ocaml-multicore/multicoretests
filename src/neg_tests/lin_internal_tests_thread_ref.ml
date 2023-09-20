open Lin_internal_tests_common

(** This is a driver of the negative ref tests over the Thread module *)

module RT_int_thread = Lin_thread.Make_internal(RConf_int) [@alert "-internal"]
module RT_int64_thread = Lin_thread.Make_internal(struct
    include RConf_int64
    let shrink_cmd = QCheck.Shrink.nil
  end ) [@alert "-internal"]

;;
if Sys.backend_type = Sys.Bytecode
then
  Printf.printf "Lin.Internal ref tests with Thread disabled under bytecode\n\n%!"
else
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [RT_int_thread.lin_test       ~count       ~name:"Lin.Internal ref int test with Thread"; (* unboxed, hence no allocations to trigger context switch *)
    RT_int64_thread.neg_lin_test ~count:15000 ~name:"Lin.Internal ref int64 test with Thread"])
