(** parallel STM tests of Weak hashsets *)

module WeakHashsetSTM_seq = STM_sequential.Make(Stm_tests_hashset_spec)
module WeakHashsetSTM_dom = STM_domain.Make(Stm_tests_hashset_spec)

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
let status_seq =
  run_tests
    [ WeakHashsetSTM_seq.agree_test         ~count:1000 ~name:"STM Weak HashSet test sequential" ]
let () = Gc.full_major ()
let status_par =
  run_tests
    [ WeakHashsetSTM_dom.neg_agree_test_par ~count:5000 ~name:"STM Weak HashSet test parallel" ]
let () = Gc.full_major ()
let status_stress =
  run_tests
    [ WeakHashsetSTM_dom.stress_test_par    ~count:1000 ~name:"STM Weak HashSet stress test parallel"; ]
let _ = exit (if status_seq=0 && status_par=0 && status_stress=0 then 0 else 1)
