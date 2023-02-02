open QCheck
open Lin.Internal [@@alert "-internal"]

module Spec =
  struct
    type t = int Stack.t

    type cmd =
      | Push of Var.t * int
      | Pop of Var.t
      | Pop_opt of Var.t
      | Top of Var.t
      | Top_opt of Var.t
      | Clear of Var.t
      | Is_empty of Var.t
      | Fold of Var.t * fct * int
      | Length of Var.t [@@deriving show { with_path = false }]
    and fct = (int -> int -> int) fun_ [@printer fun fmt f -> fprintf fmt "%s" (Fn.print f)]

    let gen_int = Gen.nat
    let gen_fct = (fun2 Observable.int Observable.int small_int).gen

    let gen_cmd gen_var =
      Gen.(oneof [
          map2 (fun t i -> None,Push (t,i)) gen_var gen_int;
          map  (fun t -> None, Pop t) gen_var;
          map  (fun t -> None, Pop_opt t) gen_var;
          map  (fun t -> None, Top t) gen_var;
          map  (fun t -> None, Top_opt t) gen_var;
          map  (fun t -> None, Clear t) gen_var;
          map  (fun t -> None, Is_empty t) gen_var;
          map3 (fun t f i -> None,Fold (t,f,i)) gen_var gen_fct gen_int;
          map  (fun t -> None, Length t) gen_var;
        ])

    let shrink_cmd _env c = match c with
      | Pop _
      | Pop_opt _
      | Top _
      | Top_opt _
      | Clear _
      | Is_empty _
      | Length _ -> Iter.empty
      | Push (t,i) -> Iter.map (fun i -> Push (t,i)) (Shrink.int i)
      | Fold (t,f,i) ->
          Iter.(
            (map (fun f -> Fold (t,f,i)) (Fn.shrink f))
            <+>
            (map (fun i -> Fold (t,f,i)) (Shrink.int i)))

    let cmd_uses_var v c = match c with
      | Push (i,_)
      | Pop i
      | Pop_opt i
      | Top i
      | Top_opt i
      | Clear i
      | Is_empty i
      | Fold (i,_,_)
      | Length i -> i=v

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
      | None,Push (t,i)   -> Stack.push i s.(t); RPush
      | None,Pop t        -> RPop (Util.protect Stack.pop s.(t))
      | None,Pop_opt t    -> RPop_opt (Stack.pop_opt s.(t))
      | None,Top t        -> RTop (Util.protect Stack.top s.(t))
      | None,Top_opt t    -> RTop_opt (Stack.top_opt s.(t))
      | None,Clear t      -> Stack.clear s.(t); RClear
      | None,Is_empty t   -> RIs_empty (Stack.is_empty s.(t))
      | None,Fold (t,f,a) -> RFold (Stack.fold (Fn.apply f) a s.(t))
      | None,Length t     -> RLength (Stack.length s.(t))
      | _, _ -> failwith (Printf.sprintf "unexpected command: %s" (show_cmd (snd c)))
  end

module SMutexConf =
  struct
    include Spec
    let m = Mutex.create ()
    let run c s = match c with
      | None,Push (t,i)   -> Mutex.lock m;
                             Stack.push i s.(t);
                             Mutex.unlock m; RPush
      | None,Pop t        -> Mutex.lock m;
                             let r = Util.protect Stack.pop s.(t) in
                             Mutex.unlock m;
                             RPop r
      | None,Pop_opt t    -> Mutex.lock m;
                             let r = Stack.pop_opt s.(t) in
                             Mutex.unlock m;
                             RPop_opt r
      | None,Top t        -> Mutex.lock m;
                             let r = Util.protect Stack.top s.(t) in
                             Mutex.unlock m;
                             RTop r
      | None,Top_opt t    -> Mutex.lock m;
                             let r = Stack.top_opt s.(t) in
                             Mutex.unlock m;
                             RTop_opt r
      | None,Clear t      -> Mutex.lock m;
                             Stack.clear s.(t);
                             Mutex.unlock m;
                             RClear
      | None,Is_empty t   -> Mutex.lock m;
                             let b = Stack.is_empty s.(t) in
                             Mutex.unlock m;
                             RIs_empty b
      | None,Fold (t,f,a) -> Mutex.lock m;
                             let r  = Stack.fold (Fn.apply f) a s.(t) in
                             Mutex.unlock m;
                             RFold r
      | None,Length t     -> Mutex.lock m;
                             let l = Stack.length s.(t) in
                             Mutex.unlock m;
                             RLength l
      | _, _ -> failwith (Printf.sprintf "unexpected command: %s" (show_cmd (snd c)))
  end

module ST_domain = Lin_domain.Make_internal(SConf) [@alert "-internal"]
module ST_thread = Lin_thread.Make_internal(SConf) [@alert "-internal"]
module SMT_domain = Lin_domain.Make_internal(SMutexConf) [@alert "-internal"]
module SMT_thread = Lin_thread.Make_internal(SMutexConf) [@alert "-internal"]
;;
QCheck_base_runner.run_tests_main [
    SMT_domain.lin_test    ~count:1000 ~name:"Lin Stack test with Domain and mutex";
    SMT_thread.lin_test    ~count:1000 ~name:"Lin Stack test with Thread and mutex";
    ST_domain.neg_lin_test ~count:1000 ~name:"Lin Stack test with Domain without mutex";
    ST_thread.lin_test     ~count:1000 ~name:"Lin Stack test with Thread without mutex";
  ]
