open QCheck
open Lin

module Spec =
  struct
    type t = int Queue.t

    type cmd =
      | Add of Var.t * int
      | Take of Var.t
      | Take_opt of Var.t
      | Peek of Var.t
      | Peek_opt of Var.t
      | Clear of Var.t
      | Is_empty of Var.t
      | Fold of Var.t * fct * int
      | Length of Var.t [@@deriving show { with_path = false }]
    and fct = (int -> int -> int) fun_ [@printer fun fmt f -> fprintf fmt "%s" (Fn.print f)]

    let gen_int = Gen.nat
    let gen_fct = (fun2 Observable.int Observable.int small_int).gen
    let gen_cmd gen_var =
      Gen.(oneof [
          map2 (fun t i -> None,Add (t,i)) gen_var gen_int;
          map  (fun t -> None, Take t) gen_var;
          map  (fun t -> None, Take_opt t) gen_var;
          map  (fun t -> None, Peek t) gen_var;
          map  (fun t -> None, Peek_opt t) gen_var;
          map  (fun t -> None, Clear t) gen_var;
          map  (fun t -> None, Is_empty t) gen_var;
          map3 (fun t f i -> None,Fold (t,f,i)) gen_var gen_fct gen_int;
          map  (fun t -> None, Length t) gen_var;
        ])

    let shrink_cmd c = match c with
      | Take _
      | Take_opt _
      | Peek _
      | Peek_opt _
      | Clear _
      | Is_empty _
      | Length _ -> Iter.empty
      | Add (t,i) -> Iter.map (fun i -> Add (t,i)) (Shrink.int i)
      | Fold (t,f,i) ->
          Iter.(
            (map (fun f -> Fold (t,f,i)) (Fn.shrink f))
            <+>
            (map (fun i -> Fold (t,f,i)) (Shrink.int i)))

    let fix_cmd env = function
      | Add (i,x)    -> Iter.map (fun i -> Add (i,x)   ) (Env.valid_t_vars env i)
      | Take i       -> Iter.map (fun i -> Take i      ) (Env.valid_t_vars env i)
      | Take_opt i   -> Iter.map (fun i -> Take_opt i  ) (Env.valid_t_vars env i)
      | Peek i       -> Iter.map (fun i -> Peek i      ) (Env.valid_t_vars env i)
      | Peek_opt i   -> Iter.map (fun i -> Peek_opt i  ) (Env.valid_t_vars env i)
      | Clear i      -> Iter.map (fun i -> Clear i     ) (Env.valid_t_vars env i)
      | Is_empty i   -> Iter.map (fun i -> Is_empty i  ) (Env.valid_t_vars env i)
      | Fold (i,x,y) -> Iter.map (fun i -> Fold (i,x,y)) (Env.valid_t_vars env i)
      | Length i     -> Iter.map (fun i -> Length i    ) (Env.valid_t_vars env i)

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
      | None,Add (t,i)    -> Queue.add i q.(t); RAdd
      | None,Take t       -> RTake (Util.protect Queue.take q.(t))
      | None,Take_opt t   -> RTake_opt (Queue.take_opt q.(t))
      | None,Peek t       -> RPeek (Util.protect Queue.peek q.(t))
      | None,Peek_opt t   -> RPeek_opt (Queue.peek_opt q.(t))
      | None,Length t     -> RLength (Queue.length q.(t))
      | None,Is_empty t   -> RIs_empty (Queue.is_empty q.(t))
      | None,Fold (t,f,a) -> RFold (Queue.fold (Fn.apply f) a q.(t))
      | None,Clear t      -> Queue.clear q.(t); RClear
      | _, _ -> failwith (Printf.sprintf "unexpected command: %s" (show_cmd (snd c)))
   end

module QMutexConf =
  struct
    include Spec
    let m = Mutex.create ()
    let run c q = match c with
      | None,Add (t,i)    -> Mutex.lock m;
                             Queue.add i q.(t);
                             Mutex.unlock m; RAdd
      | None,Take t       -> Mutex.lock m;
                             let r = Util.protect Queue.take q.(t) in
                             Mutex.unlock m;
                             RTake r
      | None,Take_opt t   -> Mutex.lock m;
                             let r = Queue.take_opt q.(t) in
                             Mutex.unlock m;
                             RTake_opt r
      | None,Peek t       -> Mutex.lock m;
                             let r = Util.protect Queue.peek q.(t) in
                             Mutex.unlock m;
                             RPeek r
      | None,Peek_opt t   -> Mutex.lock m;
                             let r = Queue.peek_opt q.(t) in
                             Mutex.unlock m;
                             RPeek_opt r
      | None,Length t     -> Mutex.lock m;
                             let l = Queue.length q.(t) in
                             Mutex.unlock m;
                             RLength l
      | None,Is_empty t   -> Mutex.lock m;
                             let b = Queue.is_empty q.(t) in
                             Mutex.unlock m;
                             RIs_empty b
      | None,Fold (t,f,a) -> Mutex.lock m;
                             let r = (Queue.fold (Fn.apply f) a q.(t))  in
                             Mutex.unlock m;
                             RFold r
      | None,Clear t      -> Mutex.lock m;
                             Queue.clear q.(t);
                             Mutex.unlock m;
                             RClear
      | _, _ -> failwith (Printf.sprintf "unexpected command: %s" (show_cmd (snd c)))
end

module QMT = Lin.Make(QMutexConf)
module QT  = Lin.Make(QConf)
;;
Util.set_ci_printing ()
;;
QCheck_base_runner.run_tests_main [
    QMT.lin_test    `Domain ~count:1000 ~name:"Lin Queue test with Domain and mutex";
    QMT.lin_test    `Thread ~count:1000 ~name:"Lin Queue test with Thread and mutex";
    QT.neg_lin_test `Domain ~count:1000 ~name:"Lin Queue test with Domain without mutex";
    QT.lin_test     `Thread ~count:1000 ~name:"Lin Queue test with Thread without mutex";
  ]
