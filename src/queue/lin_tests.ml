open QCheck
open Lin_base.Lin_internal

module Spec =
  struct
    type t = int Queue.t
    let m = Mutex.create ()

    type cmd =
      | Add of int'
      | Take
      | Take_opt
      | Peek
      | Peek_opt
      | Clear
      | Is_empty
      | Fold of fct * int'
      | Length [@@deriving qcheck, show { with_path = false }]
    and int' = int [@gen Gen.nat]
    and fct = (int -> int -> int) fun_ [@printer fun fmt f -> fprintf fmt "%s" (Fn.print f)] [@gen (fun2 Observable.int Observable.int small_int).gen]

    let shrink_cmd c = match c with
      | Take
      | Take_opt
      | Peek
      | Peek_opt
      | Clear
      | Is_empty
      | Length -> Iter.empty
      | Add i -> Iter.map (fun i -> Add i) (Shrink.int i)
      | Fold (f,i) ->
          Iter.(
            (map (fun f -> Fold (f,i)) (Fn.shrink f))
            <+>
            (map (fun i -> Fold (f,i)) (Shrink.int i)))

    type res =
      | RAdd
      | RTake of ((int, exn) result [@equal (=)])
      | RTake_opt of int option
      | RPeek of ((int, exn) result [@equal (=)])
      | RPeek_opt of int option
      | RClear
      | RIs_empty of bool
      | RFold of int
      | RLength of int [@@deriving show { with_path = false }, eq]

    let init () = Queue.create ()
    let cleanup _ = ()
  end

module QConf =
  struct
    include Spec
    let run c q = match c with
      | Add i       -> Queue.add i q; RAdd
      | Take        -> RTake (Util.protect Queue.take q)
      | Take_opt    -> RTake_opt (Queue.take_opt q)
      | Peek        -> RPeek (Util.protect Queue.peek q)
      | Peek_opt    -> RPeek_opt (Queue.peek_opt q)
      | Length      -> RLength (Queue.length q)
      | Is_empty    -> RIs_empty (Queue.is_empty q)
      | Fold (f, a) -> RFold (Queue.fold (Fn.apply f) a q)
      | Clear       -> Queue.clear q; RClear
   end

module QMutexConf =
  struct
    include Spec
    let run c q = match c with
      | Add i       -> Mutex.lock m;
                       Queue.add i q;
                       Mutex.unlock m; RAdd
      | Take        -> Mutex.lock m;
                       let r = Util.protect Queue.take q in
                       Mutex.unlock m;
                       RTake r
      | Take_opt    -> Mutex.lock m;
                       let r = Queue.take_opt q in
                       Mutex.unlock m;
                       RTake_opt r
      | Peek        -> Mutex.lock m;
                       let r = Util.protect Queue.peek q in
                       Mutex.unlock m;
                       RPeek r
      | Peek_opt    -> Mutex.lock m;
                       let r = Queue.peek_opt q in
                       Mutex.unlock m;
                       RPeek_opt r
      | Length      -> Mutex.lock m;
                       let l = Queue.length q in
                       Mutex.unlock m;
                       RLength l
      | Is_empty    -> Mutex.lock m;
                       let b = Queue.is_empty q in
                       Mutex.unlock m;
                       RIs_empty b
      | Fold (f, a) -> Mutex.lock m;
                       let r = (Queue.fold (Fn.apply f) a q)  in
                       Mutex.unlock m;
                       RFold r
      | Clear       -> Mutex.lock m;
                       Queue.clear q;
                       Mutex.unlock m;
                       RClear
end

module QMT_domain = Lin_domain.Make_internal(QMutexConf)
module QMT_thread = Lin_thread.Make_internal(QMutexConf)
module QT_domain  = Lin_domain.Make_internal(QConf)
module QT_thread  = Lin_thread.Make_internal(QConf)
;;
QCheck_base_runner.run_tests_main [
    QMT_domain.lin_test    ~count:1000 ~name:"Lin Queue test with Domain and mutex";
    QMT_thread.lin_test    ~count:1000 ~name:"Lin Queue test with Thread and mutex";
    QT_domain.neg_lin_test ~count:1000 ~name:"Lin Queue test with Domain without mutex";
    QT_thread.lin_test     ~count:1000 ~name:"Lin Queue test with Thread without mutex";
  ]
