(** parallel STM tests of Ephemeron *)

module ETest_seq = STM_sequential.Make(Stm_tests_spec)
module ETest_dom = STM_domain.Make(Stm_tests_spec)

(* Beware: hoop jumping to enable a full major Gc run between the two tests!
   We need that to avoid the state of the second test depending on the resulting
   GC state of the first test and don't want to exit after the first run
   (as QCheck_base_runner.run_tests_main does). *)
let cli_args = QCheck_base_runner.Raw.parse_cli ~full_options:false Sys.argv
let run_tests l =
  QCheck_base_runner.run_tests l
    ~colors:cli_args.cli_colors
    ~verbose:cli_args.cli_verbose
    ~long:cli_args.cli_long_tests ~out:stdout ~rand:cli_args.cli_rand
let count = 1000
let status_seq =
  run_tests
    [ ETest_seq.agree_test         ~count ~name:"STM Ephemeron test sequential"; ]
let () = Gc.full_major ()
let status_par =
  run_tests
    [ ETest_dom.neg_agree_test_par ~count ~name:"STM Ephemeron test parallel"; ]
let _ = exit (if status_seq=0 && status_par=0 then 0 else 1)
