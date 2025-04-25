(* parallel stress tests of the GC with explicit Gc invocations *)

module GC_STM_dom = STM_domain.Make(Stm_tests_spec)

let _ =
  Printf.printf "Page size: %i\n" (Pagesize.get ());
  QCheck_base_runner.run_tests_main [
    GC_STM_dom.stress_test_par    ~count:2000 ~name:"STM Gc stress test parallel";
  ]
