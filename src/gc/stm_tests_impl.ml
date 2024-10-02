(* parallel tests of the GC, without explicit Gc invocations *)

module ImplGCConf =
struct
  include GCConf
  let arb_cmd = arb_alloc_cmd
end

module GC_STM_dom = STM_domain.Make(ImplGCConf)

let _ =
  Printf.printf "Page size: %i\n" (Pagesize.get ());
  QCheck_base_runner.run_tests_main [
    GC_STM_dom.agree_test_par  ~count:1000 ~name:"STM implicit Gc test parallel";
  ]
