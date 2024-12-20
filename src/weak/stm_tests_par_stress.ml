(** parallel stress STM tests of Weak arrays *)

module WeakSTM_dom = STM_domain.Make(Stm_tests_spec_weak)

let _ =
  QCheck_base_runner.run_tests_main
    [ WeakSTM_dom.stress_test_par ~count:1000 ~name:"STM Weak stress test parallel" ]
