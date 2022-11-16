module Queue_spec : Lin_base.ApiSpec = struct
  open Lin_base
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
module Lin_queue_thread = Lin_thread.Make(Queue_spec)

let () =
  QCheck_base_runner.run_tests_main [
    Lin_queue_domain.neg_lin_test ~count:1000 ~name:"Lin DSL Queue test with Domain";
    Lin_queue_thread.lin_test     ~count:250  ~name:"Lin DSL Queue test with Thread";
  ]
