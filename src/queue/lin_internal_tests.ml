open QCheck

module Spec =
  struct
    type t = int Queue.t
    let m = Mutex.create ()

    type cmd =
      | Add of int
      | Take
      | Take_opt
      | Peek
      | Peek_opt
      | Clear
      | Is_empty
      | Fold of fct * int
      | Length
    and fct = (int -> int -> int) fun_

    let pp_cmd par fmt x =
      let open Util.Pp in
      let pp_fct = of_show Fn.print in
      match x with
      | Add x -> cst1 pp_int "Add" par fmt x
      | Take -> cst0 "Take" fmt
      | Take_opt -> cst0 "Take_opt" fmt
      | Peek -> cst0 "Peek" fmt
      | Peek_opt -> cst0 "Peek_opt" fmt
      | Clear -> cst0 "Clear" fmt
      | Is_empty -> cst0 "Is_empty" fmt
      | Fold (x, y) -> cst2 pp_fct pp_int "Fold" par fmt x y
      | Length -> cst0 "Length" fmt

    let show_cmd = Util.Pp.to_show pp_cmd

    let gen_cmd =
      let open QCheck.Gen in
      let int = nat
      and fct = (fun2 Observable.int Observable.int QCheck.small_int).gen in
      oneof
        [
          map (fun x -> Add x) int;
          pure Take;
          pure Take_opt;
          pure Peek;
          pure Peek_opt;
          pure Clear;
          pure Is_empty;
          map2 (fun x y -> Fold (x, y)) fct int;
          pure Length;
        ]

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
      | RTake of (int, exn) result
      | RTake_opt of int option
      | RPeek of (int, exn) result
      | RPeek_opt of int option
      | RClear
      | RIs_empty of bool
      | RFold of int
      | RLength of int

    let pp_res par fmt x =
      let open Util.Pp in
      match x with
      | RAdd -> cst0 "RAdd" fmt
      | RTake x -> cst1 (pp_result pp_int pp_exn) "RTake" par fmt x
      | RTake_opt x -> cst1 (pp_option pp_int) "RTake_opt" par fmt x
      | RPeek x -> cst1 (pp_result pp_int pp_exn) "RPeek" par fmt x
      | RPeek_opt x -> cst1 (pp_option pp_int) "RPeek_opt" par fmt x
      | RClear -> cst0 "RClear" fmt
      | RIs_empty x -> cst1 pp_bool "RIs_empty" par fmt x
      | RFold x -> cst1 pp_int "RFold" par fmt x
      | RLength x -> cst1 pp_int "RLength" par fmt x

    let show_res = Util.Pp.to_show pp_res

    let equal_res x y =
      let open Util.Equal in
      match (x, y) with
      | RAdd, RAdd -> true
      | RTake x, RTake y -> equal_result equal_int equal_exn x y
      | RTake_opt x, RTake_opt y -> equal_option equal_int x y
      | RPeek x, RPeek y -> equal_result equal_int equal_exn x y
      | RPeek_opt x, RPeek_opt y -> equal_option equal_int x y
      | RClear, RClear -> true
      | RIs_empty x, RIs_empty y -> equal_bool x y
      | RFold x, RFold y -> equal_int x y
      | RLength x, RLength y -> equal_int x y
      | _, _ -> false

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

module QMT_domain = Lin_domain.Make_internal(QMutexConf) [@alert "-internal"]
module QMT_thread = Lin_thread.Make_internal(QMutexConf) [@alert "-internal"]
module QT_domain  = Lin_domain.Make_internal(QConf) [@alert "-internal"]
module QT_thread  = Lin_thread.Make_internal(QConf) [@alert "-internal"]
;;
QCheck_base_runner.run_tests_main [
    QMT_domain.lin_test    ~count:1000 ~name:"Lin.Internal Queue test with Domain and mutex";
    QMT_thread.lin_test    ~count:1000 ~name:"Lin.Internal Queue test with Thread and mutex";
    QT_domain.neg_lin_test ~count:1000 ~name:"Lin.Internal Queue test with Domain without mutex";
    QT_thread.lin_test     ~count:1000 ~name:"Lin.Internal Queue test with Thread without mutex";
  ]
