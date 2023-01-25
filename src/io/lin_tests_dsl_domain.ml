(* ********************************************************************** *)
(*                      Tests of in and out channels                      *)
(* ********************************************************************** *)

open Lin_tests_dsl_common_io.Lin_tests_dsl_common

module IC_domain = Lin_domain.Make(ICConf)
module OC_domain = Lin_domain.Make(OCConf)

let _ =
  QCheck_base_runner.run_tests_main [
    IC_domain.neg_lin_test ~count:1000 ~name:"Lin DSL In_channel test with Domain";
    OC_domain.neg_lin_test ~count:1000 ~name:"Lin DSL Out_channel test with Domain";
  ]
