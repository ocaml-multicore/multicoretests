(* ********************************************************************** *)
(*                      Tests of in and out channels                      *)
(* ********************************************************************** *)

open Lin_tests_dsl_common_io.Lin_tests_dsl_common

module IC_thread = Lin_thread.Make(ICConf) [@@alert "-experimental"]
module OC_thread = Lin_thread.Make(OCConf) [@@alert "-experimental"]

let _ =
  QCheck_base_runner.run_tests_main [
    IC_thread.neg_lin_test ~count:1000 ~name:"Lin DSL In_channel test with Thread";
    OC_thread.neg_lin_test ~count:1000 ~name:"Lin DSL Out_channel test with Thread";
  ]
