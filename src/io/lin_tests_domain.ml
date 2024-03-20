(* ********************************************************************** *)
(*                      Tests of In_channels                              *)
(* ********************************************************************** *)

module IC_domain = Lin_domain.Make(Lin_tests_spec_io.ICConf)

let _ =
  QCheck_base_runner.run_tests_main [
    IC_domain.neg_lin_test ~count:1000 ~name:"Lin In_channel test with Domain";
    IC_domain.stress_test  ~count:1000 ~name:"Lin In_channel stress test with Domain";
  ]
