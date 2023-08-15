module Lin_queue_domain = Lin_domain.Make(Lin_tests_spec_queue)
module Lin_queue_thread = Lin_thread.Make(Lin_tests_spec_queue) [@alert "-experimental"]

let () =
  let tests = [
    Lin_queue_domain.neg_lin_test ~count:1000 ~name:"Lin Queue test with Domain";
    Lin_queue_thread.lin_test     ~count:250  ~name:"Lin Queue test with Thread";
  ] in
  let tests =
    if Sys.backend_type = Sys.Bytecode then (
      Printf.printf "Lin Queue test with Thread disabled under bytecode\n\n%!";
      [ List.hd tests ])
    else tests
  in
  QCheck_base_runner.run_tests_main tests
