open QCheck
open Lin_base.Lin_internal

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
      | Fold of fct * int'
      | Length [@@deriving qcheck, show { with_path = false }]
    and int' = int [@gen Gen.nat]
    and fct = (int -> int -> int) fun_ [@printer fun fmt f -> fprintf fmt "%s" (Fn.print f)] [@gen (fun2 Observable.int Observable.int small_int).gen]

    let shrink_cmd c = match c with
      | Pop
      | Pop_opt
      | Top
      | Top_opt
      | Clear
      | Is_empty
      | Length -> Iter.empty
      | Push i -> Iter.map (fun i -> Push i) (Shrink.int i)
      | Fold (f,i) ->
          Iter.(
            (map (fun f -> Fold (f,i)) (Fn.shrink f))
            <+>
            (map (fun i -> Fold (f,i)) (Shrink.int i)))

    type res =
      | RPush
      | RPop of ((int, exn) result [@equal (=)])
      | RPop_opt of int option
      | RTop of ((int, exn) result [@equal (=)])
      | RTop_opt of int option
      | RClear
      | RIs_empty of bool
      | RFold of int
      | RLength of int [@@deriving show { with_path = false }, eq]

    let init () = Stack.create ()
    let cleanup _ = ()
  end

module SConf =
  struct
    include Spec
    let run c s = match c with
      | Push i      -> Stack.push i s; RPush
      | Pop         -> RPop (Util.protect Stack.pop s)
      | Pop_opt     -> RPop_opt (Stack.pop_opt s)
      | Top         -> RTop (Util.protect Stack.top s)
      | Top_opt     -> RTop_opt (Stack.top_opt s)
      | Clear       -> Stack.clear s; RClear
      | Is_empty    -> RIs_empty (Stack.is_empty s)
      | Fold (f, a) -> RFold (Stack.fold (Fn.apply f) a s)
      | Length      -> RLength (Stack.length s)
  end

module SMutexConf =
  struct
    include Spec
    let run c s = match c with
      | Push i      -> Mutex.lock m;
                       Stack.push i s;
                       Mutex.unlock m; RPush
      | Pop         -> Mutex.lock m;
                       let r = Util.protect Stack.pop s in
                       Mutex.unlock m;
                       RPop r
      | Pop_opt     -> Mutex.lock m;
                       let r = Stack.pop_opt s in
                       Mutex.unlock m;
                       RPop_opt r
      | Top         -> Mutex.lock m;
                       let r = Util.protect Stack.top s in
                       Mutex.unlock m;
                       RTop r
      | Top_opt     -> Mutex.lock m;
                       let r = Stack.top_opt s in
                       Mutex.unlock m;
                       RTop_opt r
      | Clear       -> Mutex.lock m;
                       Stack.clear s;
                       Mutex.unlock m;
                       RClear
      | Is_empty    -> Mutex.lock m;
                       let b = Stack.is_empty s in
                       Mutex.unlock m;
                       RIs_empty b
      | Fold (f, a) -> Mutex.lock m;
                       let r  = Stack.fold (Fn.apply f) a s in
                       Mutex.unlock m;
                       RFold r
      | Length      -> Mutex.lock m;
                       let l = Stack.length s in
                       Mutex.unlock m;
                       RLength l
  end

module ST_domain = Lin_domain.Make_internal(SConf)
module ST_thread = Lin_thread.Make_internal(SConf)
module SMT_domain = Lin_domain.Make_internal(SMutexConf)
module SMT_thread = Lin_thread.Make_internal(SMutexConf)
;;
QCheck_base_runner.run_tests_main [
    SMT_domain.lin_test    ~count:1000 ~name:"Lin Stack test with Domain and mutex";
    SMT_thread.lin_test    ~count:1000 ~name:"Lin Stack test with Thread and mutex";
    ST_domain.neg_lin_test ~count:1000 ~name:"Lin Stack test with Domain without mutex";
    ST_thread.lin_test     ~count:1000 ~name:"Lin Stack test with Thread without mutex";
  ]
