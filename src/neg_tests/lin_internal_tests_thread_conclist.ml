open Lin_internal_tests_common

(** This is a driver of the negative CList tests over the Thread module *)
module CList_bis = struct include CList let add_node = add_node_thread end
module CLT_int_thread = Lin_thread.Make_internal(CLConf(CList_bis)(Int)) [@alert "-internal"]
module CLT_int64_thread = Lin_thread.Make_internal(CLConf(CList_bis)(Int64)) [@alert "-internal"]

let _ =
  if Sys.backend_type = Sys.Bytecode then
    Printf.printf "Lin.Internal CList tests with Thread disabled under bytecode\n\n%!"
  else
    let count = 1000 in
    QCheck_base_runner.run_tests_main [
      CLT_int_thread.neg_lin_test   ~count ~name:"Lin.Internal CList int test with Thread";
      CLT_int64_thread.neg_lin_test ~count ~name:"Lin.Internal CList int64 test with Thread"
    ]
