module Stack_spec : Lin_api.ApiSpec = struct
    open Lin_api
    type t = int Stack.t
    let init () = Stack.create ()
    let cleanup _ = ()
    let api =
      [ val_ "Stack.push"     Stack.push (int @-> t @-> returning unit);
        val_ "Stack.pop"      Stack.pop (t @-> returning_or_exc int);
        val_ "Stack.pop_opt"  Stack.pop_opt (t @-> returning (option int));
        val_ "Stack.top"      Stack.top (t @-> returning_or_exc int);
        val_ "Stack.top_opt"  Stack.top_opt (t @-> returning (option int));
        val_ "Stack.clear"    Stack.clear (t @-> returning unit);
        val_ "Stack.is_empty" Stack.is_empty (t @-> returning bool);
        val_ "Stack.length"   Stack.length (t @-> returning int);
        (* val_ "Stack.fold" Stack.fold (t @-> missing function type in the api ... *)
      ]
  end

module Lin_stack = Lin_api.Make(Stack_spec)

let () =
  let count,name = 1000,"Stack test" in
  QCheck_runner.run_tests_main [
      Lin_stack.neg_lin_test ~count ~name `Domain;
      Lin_stack.lin_test     ~count ~name `Thread;
    ]
