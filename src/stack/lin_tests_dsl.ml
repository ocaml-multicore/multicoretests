module Stack_spec : Lin_base.ApiSpec = struct
    open Lin_base
    type t = int Stack.t
    let init () = Stack.create ()
    let cleanup _ = ()
    let int = int_small
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

module Stack_domain = Lin_domain.Make(Stack_spec)
module Stack_thread = Lin_thread.Make(Stack_spec)

let () =
  QCheck_base_runner.run_tests_main [
      Stack_domain.neg_lin_test ~count:1000 ~name:"Lin_api Stack test with Domain";
      Stack_thread.lin_test     ~count:250  ~name:"Lin_api Stack test with Thread";
    ]
