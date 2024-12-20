(** parallel STM tests of Ephemeron *)

module ETest_dom = STM_domain.Make(Stm_tests_spec)

let _ =
  QCheck_base_runner.run_tests_main
    [ ETest_dom.neg_agree_test_par ~count:1000 ~name:"STM Ephemeron test parallel" ]
