(* ********************************************************************** *)
(*                      Tests of in and out channels                      *)
(* ********************************************************************** *)

module IC_thread = Lin_thread.Make(Lin_tests_spec_io.ICConf) [@@alert "-experimental"]
module OC_thread = Lin_thread.Make(Lin_tests_spec_io.OCConf) [@@alert "-experimental"]

let _ =
  QCheck_base_runner.run_tests_main [
    IC_thread.neg_lin_test ~count:1000 ~name:"Lin In_channel test with Thread";
    OC_thread.neg_lin_test ~count:1000 ~name:"Lin Out_channel test with Thread";
  ]
