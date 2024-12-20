(** parallel STM tests of Weak arrays *)

module WeakSTM_dom = STM_domain.Make(Stm_tests_weak_spec)

let _ =
  QCheck_base_runner.run_tests_main
    [ WeakSTM_dom.neg_agree_test_par ~count:5000 ~name:"STM Weak test parallel" ]
