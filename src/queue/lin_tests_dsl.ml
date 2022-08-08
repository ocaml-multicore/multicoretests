module Queue_spec : Lin_api.ApiSpec = struct
  open Lin_api
    type t = int Queue.t
    let init () = Queue.create ()
    let cleanup _ = ()
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

module Lin_queue = Lin_api.Make(Queue_spec)

let () =
  let count,name = 1000,"Queue test" in
  QCheck_runner.run_tests_main [
    Lin_queue.neg_lin_test ~count ~name `Domain;
    Lin_queue.lin_test     ~count ~name `Thread;
  ]
