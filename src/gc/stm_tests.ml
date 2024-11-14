open QCheck

(* sequential and parallel tests of the GC with explicit Gc invocations *)

module GC_STM_seq = STM_sequential.Make(GCConf)
module GC_STM_dom = STM_domain.Make(GCConf)
(*
let agree_prop cs = match Util.protect GC_STM_seq.agree_prop cs with
  | Ok r -> r
  | Error Stack_overflow -> true (* Stack_overflow is accepted behaviour *)
  | Error e -> raise e
*)
(* Run seq. property in a child domain to stresstest parent-child GC *)
let agree_child_prop cs = match Domain.spawn (fun () -> Util.protect GC_STM_seq.agree_prop cs) |> Domain.join with
  | Ok r -> r
  | Error Stack_overflow -> true (* Stack_overflow is accepted behaviour *)
  | Error e -> raise e
(*
let agree_test ~count ~name =
  Test.make ~name ~count (GC_STM_seq.arb_cmds GCConf.init_state) agree_prop
*)
let agree_child_test ~count ~name =
  Test.make ~name ~count (GC_STM_seq.arb_cmds GCConf.init_state) agree_child_prop

let _ =
  Printf.printf "Page size: %i\n" (Pagesize.get ());
  QCheck_base_runner.run_tests_main [
  (*agree_test                    ~count:1000 ~name:"STM Gc test sequential";*)
    agree_child_test              ~count:1000 ~name:"STM Gc test sequential in child domain";
    GC_STM_dom.neg_agree_test_par ~count:1000 ~name:"STM Gc test parallel";
    GC_STM_dom.stress_test_par    ~count:1000 ~name:"STM Gc stress test parallel";
  ]
