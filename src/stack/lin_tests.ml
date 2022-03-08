open QCheck

module Spec =
  struct
    type t = int Stack.t
    let m = Mutex.create ()
    
    type cmd =
      | Push of int'
      | Pop
      | Pop_opt
      | Top
      | Top_opt
      | Clear
      | Is_empty
      | Length [@@deriving qcheck, show { with_path = false }]
    and int' = int [@gen Gen.nat]

    type res =
      | RPush
      | RPop of int option
      | RPop_opt of int option
      | RTop of int option
      | RTop_opt of int option
      | RClear
      | RIs_empty of bool
      | RLength of int [@@deriving show { with_path = false }]

    let init () = Stack.create ()
    let cleanup _ = ()
  end

module SConf =
  struct
    include Spec
    let run c q = match c with
      | Push i   -> Stack.push i q; RPush
      | Length   -> RLength (Stack.length q) 
      | Clear    -> Stack.clear q; RClear
      | Is_empty -> RIs_empty (Stack.is_empty q) 
      | Top      -> RTop (
                      try Some (Stack.top q)
                      with Stack.Empty -> None)
      | Top_opt  -> RTop_opt (Stack.top_opt q) 
      | Pop      -> RPop (
                      try Some (Stack.pop q)
                      with Stack.Empty -> None)
      | Pop_opt  -> RPop_opt (Stack.pop_opt q)
    
  end

module SMutexConf =
  struct
    include Spec
    let run c q = match c with
      | Push i   -> Mutex.lock m;
                    Stack.push i q;
                    Mutex.unlock m; RPush
      | Length   -> Mutex.lock m;
                    let l = Stack.length q in
                    Mutex.unlock m;
                    RLength l
      | Clear    -> Mutex.lock m;
                    Stack.clear q;
                    Mutex.unlock m;
                    RClear
      | Is_empty -> Mutex.lock m;
                    let b = Stack.is_empty q in
                    Mutex.unlock m;
                    RIs_empty b
      | Top      -> Mutex.lock m;
                    let r =
                      (try Some (Stack.top q)
                       with Stack.Empty -> None)
                    in
                    Mutex.unlock m;
                    RTop r
      | Top_opt  -> Mutex.lock m;
                    let r = Stack.top_opt q in
                    Mutex.unlock m;
                    RTop_opt r
      | Pop      -> Mutex.lock m;
                    let r =
                      try Some (Stack.pop q)
                      with Stack.Empty -> None
                    in
                    Mutex.unlock m;
                    RPop r
      | Pop_opt  -> Mutex.lock m;
                    let r = Stack.pop_opt q in
                    Mutex.unlock m;
                    RPop_opt r
  end

module ST = Lin.Make(SConf)
module SMT = Lin.Make(SMutexConf)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main [
    ST.lin_test `Domain ~count:1000 ~name:"Stack test with domains without mutex";
    ST.lin_test `Thread ~count:1000 ~name:"Stack test with threads without mutex";
    SMT.lin_test `Domain ~count:1000 ~name:"Stack test with domains and mutex";
    SMT.lin_test `Thread ~count:1000 ~name:"Stack test with threads and mutex";
  ]
