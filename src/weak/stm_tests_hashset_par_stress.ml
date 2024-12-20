(** parallel STM stress tests of Weak hashsets *)

module WeakHashsetSTM_dom = STM_domain.Make(Stm_tests_hashset_spec)

let _ =
  QCheck_base_runner.run_tests_main
    [ WeakHashsetSTM_dom.stress_test_par ~count:1000 ~name:"STM Weak HashSet stress test parallel" ]
