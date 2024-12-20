(** sequential STM tests of Weak hashsets *)

module WeakHashsetSTM_seq = STM_sequential.Make(Stm_tests_hashset_spec)

let _ =
  QCheck_base_runner.run_tests_main
    [ WeakHashsetSTM_seq.agree_test ~count:1000 ~name:"STM Weak HashSet test sequential" ]
