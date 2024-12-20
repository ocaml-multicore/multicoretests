(** sequential STM tests of Weak arrays *)

module WeakSTM_seq = STM_sequential.Make(Stm_tests_weak_spec)

let _ =
  QCheck_base_runner.run_tests_main
    [ WeakSTM_seq.agree_test ~count:1000 ~name:"STM Weak test sequential" ]
