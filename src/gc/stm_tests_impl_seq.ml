open QCheck

(* sequential tests of the GC, without explicit Gc invocations *)

module ImplGCConf =
struct
  include Stm_tests_spec
  let arb_cmd = arb_alloc_cmd
end

module GC_STM_seq = STM_sequential.Make(ImplGCConf)

let agree_prop cs = match Util.protect GC_STM_seq.agree_prop cs with
  | Ok r -> r
  | Error Stack_overflow -> true (* Stack_overflow is accepted behaviour *)
  | Error e -> raise e

let agree_test ~count ~name =
  Test.make ~name ~count (GC_STM_seq.arb_cmds ImplGCConf.init_state) agree_prop

let _ =
  Printf.printf "Page size: %i\n" (Pagesize.get ());
  QCheck_base_runner.run_tests_main [
    agree_test ~count:1000 ~name:"STM implicit Gc test sequential";
  ]
