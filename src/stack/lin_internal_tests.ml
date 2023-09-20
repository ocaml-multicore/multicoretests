open QCheck

module Spec =
  struct
    type t = int Stack.t
    let m = Mutex.create ()

    type cmd =
      | Push of int
      | Pop
      | Pop_opt
      | Top
      | Top_opt
      | Clear
      | Is_empty
      | Fold of fct * int
      | Length
    and fct = (int -> int -> int) fun_

    let pp_cmd par fmt x =
      let open Util.Pp in
      let pp_fct = of_show Fn.print in
      match x with
      | Push x -> cst1 pp_int "Push" par fmt x
      | Pop -> cst0 "Pop" fmt
      | Pop_opt -> cst0 "Pop_opt" fmt
      | Top -> cst0 "Top" fmt
      | Top_opt -> cst0 "Top_opt" fmt
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
          map (fun x -> Push x) int;
          pure Pop;
          pure Pop_opt;
          pure Top;
          pure Top_opt;
          pure Clear;
          pure Is_empty;
          map2 (fun x y -> Fold (x, y)) fct int;
          pure Length;
        ]

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
      | RPop of (int, exn) result
      | RPop_opt of int option
      | RTop of (int, exn) result
      | RTop_opt of int option
      | RClear
      | RIs_empty of bool
      | RFold of int
      | RLength of int

    let pp_res par fmt x =
      let open Util.Pp in
      match x with
      | RPush -> cst0 "RPush" fmt
      | RPop x -> cst1 (pp_result pp_int pp_exn) "RPop" par fmt x
      | RPop_opt x -> cst1 (pp_option pp_int) "RPop_opt" par fmt x
      | RTop x -> cst1 (pp_result pp_int pp_exn) "RTop" par fmt x
      | RTop_opt x -> cst1 (pp_option pp_int) "RTop_opt" par fmt x
      | RClear -> cst0 "RClear" fmt
      | RIs_empty x -> cst1 pp_bool "RIs_empty" par fmt x
      | RFold x -> cst1 pp_int "RFold" par fmt x
      | RLength x -> cst1 pp_int "RLength" par fmt x

    let show_res = Util.Pp.to_show pp_res

    let equal_res x y =
      let open Util.Equal in
      match (x, y) with
      | RPush, RPush -> true
      | RPop x, RPop y -> equal_result equal_int equal_exn x y
      | RPop_opt x, RPop_opt y -> equal_option equal_int x y
      | RTop x, RTop y -> equal_result equal_int equal_exn x y
      | RTop_opt x, RTop_opt y -> equal_option equal_int x y
      | RClear, RClear -> true
      | RIs_empty x, RIs_empty y -> equal_bool x y
      | RFold x, RFold y -> equal_int x y
      | RLength x, RLength y -> equal_int x y
      | _, _ -> false

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

module ST_domain = Lin_domain.Make_internal(SConf) [@alert "-internal"]
module ST_thread = Lin_thread.Make_internal(SConf) [@alert "-internal"]
module SMT_domain = Lin_domain.Make_internal(SMutexConf) [@alert "-internal"]
module SMT_thread = Lin_thread.Make_internal(SMutexConf) [@alert "-internal"]
;;
QCheck_base_runner.run_tests_main [
    SMT_domain.lin_test    ~count:1000 ~name:"Lin.Internal Stack test with Domain and mutex";
    SMT_thread.lin_test    ~count:1000 ~name:"Lin.Internal Stack test with Thread and mutex";
    ST_domain.neg_lin_test ~count:1000 ~name:"Lin.Internal Stack test with Domain without mutex";
    ST_thread.lin_test     ~count:1000 ~name:"Lin.Internal Stack test with Thread without mutex";
  ]
