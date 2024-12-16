open QCheck

(* sequential tests of the GC with explicit Gc invocations *)

module GC_STM_seq = STM_sequential.Make(GCConf)

(* Run seq. property in a child domain to stresstest parent-child GC *)
let agree_child_prop cs = match Domain.spawn (fun () -> Util.protect GC_STM_seq.agree_prop cs) |> Domain.join with
  | Ok r -> r
  | Error Stack_overflow -> true (* Stack_overflow is accepted behaviour *)
  | Error e -> raise e

let agree_child_test ~count ~name =
  Test.make ~name ~count (GC_STM_seq.arb_cmds GCConf.init_state) agree_child_prop

let _ =
  Printf.printf "Page size: %i\n" (Pagesize.get ());
  QCheck_base_runner.run_tests_main [
    agree_child_test              ~count:1000 ~name:"STM Gc test sequential in child domain";
  ]
