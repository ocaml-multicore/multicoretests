open QCheck

(* sequential tests of the GC with explicit Gc invocations *)

module GC_STM_seq = STM_sequential.Make(Stm_tests_spec)

let agree_prop cs = match Util.protect GC_STM_seq.agree_prop cs with
  | Ok r -> r
  | Error Stack_overflow -> true (* Stack_overflow is accepted behaviour *)
  | Error e -> raise e

let agree_test ~count ~name =
  Test.make ~name ~count (GC_STM_seq.arb_cmds Stm_tests_spec.init_state) agree_prop

let _ =
  Printf.printf "Page size: %i\n" (Pagesize.get ());
  QCheck_base_runner.run_tests_main [
    agree_test ~count:1000 ~name:"STM Gc test sequential";
  ]
