module Stack_spec : Lin.Spec = struct
    open Lin
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
module Stack_thread = Lin_thread.Make(Stack_spec) [@alert "-experimental"]

let () =
  let tests = [
    Stack_domain.neg_lin_test ~count:1000 ~name:"Lin Stack test with Domain";
    Stack_domain.stress_test  ~count:1000 ~name:"Lin Stack stress test with Domain";
    Stack_thread.lin_test ~count:250 ~name:"Lin Stack test with Thread";
  ] in
  let tests =
    if Sys.backend_type = Sys.Bytecode then (
      Printf.printf "Lin Stack test with Thread disabled under bytecode\n\n%!";
      [ List.hd tests ])
    else tests
  in
  QCheck_base_runner.run_tests_main tests
