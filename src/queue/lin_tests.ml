module Queue_spec : Lin.Spec = struct
  open Lin
    type t = int Queue.t
    let init () = Queue.create ()
    let cleanup _ = ()
    let int = int_small
    let api =
      [ val_ "Queue.add"      Queue.add      (int @-> t @-> returning unit);
        val_ "Queue.take"     Queue.take     (t @-> returning_or_exc int);
        val_ "Queue.take_opt" Queue.take_opt (t @-> returning (option int));
        val_ "Queue.peek"     Queue.peek     (t @-> returning_or_exc int);
        val_ "Queue.peek_opt" Queue.peek_opt (t @-> returning (option int));
        val_ "Queue.clear"    Queue.clear    (t @-> returning unit);
        val_ "Queue.is_empty" Queue.is_empty (t @-> returning bool);
        val_ "Queue.length"   Queue.length   (t @-> returning int);
        (* val_ "Queue.fold" Queue.fold ... need function type combinator *)
      ]
end

module Lin_queue_domain = Lin_domain.Make(Queue_spec)
module Lin_queue_thread = Lin_thread.Make(Queue_spec) [@alert "-experimental"]

let () =
  let tests = [
    Lin_queue_domain.neg_lin_test ~count:1000 ~name:"Lin Queue test with Domain";
    Lin_queue_domain.stress_test  ~count:1000 ~name:"Lin Queue stress test with Domain";
    Lin_queue_thread.lin_test     ~count:250  ~name:"Lin Queue test with Thread";
  ] in
  let tests =
    if Sys.backend_type = Sys.Bytecode then (
      Printf.printf "Lin Queue test with Thread disabled under bytecode\n\n%!";
      [ List.hd tests ])
    else tests
  in
  QCheck_base_runner.run_tests_main tests
