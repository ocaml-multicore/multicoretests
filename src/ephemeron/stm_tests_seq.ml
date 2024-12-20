(** sequential STM tests of Ephemeron *)

module ETest_seq = STM_sequential.Make(Stm_tests_spec)

let _ =
  QCheck_base_runner.run_tests_main
    [ ETest_seq.agree_test ~count:1000 ~name:"STM Ephemeron test sequential" ]
