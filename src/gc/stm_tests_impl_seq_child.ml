open QCheck

(* sequential tests of the GC, without explicit Gc invocations *)

module ImplGCConf =
struct
  include GCConf
  let arb_cmd = arb_alloc_cmd
end

module GC_STM_seq = STM_sequential.Make(ImplGCConf)

(* Run seq. property in a child domain to stresstest parent-child GC *)
let agree_child_prop cs = match Domain.spawn (fun () -> Util.protect GC_STM_seq.agree_prop cs) |> Domain.join with
  | Ok r -> r
  | Error Stack_overflow -> true (* Stack_overflow is accepted behaviour *)
  | Error e -> raise e

let agree_child_test ~count ~name =
  Test.make ~name ~count (GC_STM_seq.arb_cmds ImplGCConf.init_state) agree_child_prop

let _ =
  Printf.printf "Page size: %i\n" (Pagesize.get ());
  QCheck_base_runner.run_tests_main [
    agree_child_test ~count:1000 ~name:"STM implicit Gc test sequential in child domain";
  ]
