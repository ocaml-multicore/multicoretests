open QCheck

(** ********************************************************************** *)
(**                   Tests of the Atomic module                           *)
(** ********************************************************************** *)
module AConf =
struct
  type t = int Atomic.t
  open Lin
  type cmd =
    | Make of int
    | Get of Var.t
    | Set of Var.t * int
    | Exchange of Var.t * int
    | Compare_and_set of Var.t * int * int
    | Fetch_and_add of Var.t * int
    | Incr of Var.t
    | Decr of Var.t [@@deriving show { with_path = false }]

  let gen_int = Gen.nat
  let gen_cmd gen_var =
    Gen.(oneof [
        map  (fun i ->     (Some (Var.next()), Make i)) gen_int;
        map  (fun t ->     (None, Get t)) gen_var;
        map2 (fun t i ->   (None, Set (t,i))) gen_var gen_int;
        map2 (fun t i ->   (None, Exchange (t,i))) gen_var gen_int;
        map3 (fun t i j -> (None, Compare_and_set (t,i,j))) gen_var gen_int gen_int;
        map2 (fun t i ->   (None, Fetch_and_add (t,i))) gen_var gen_int;
        map  (fun t ->     (None, Incr t)) gen_var;
        map  (fun t ->     (None, Decr t)) gen_var;
      ])

  let shrink_cmd = Shrink.nil

  let fix_cmd env = function
    | Make x as cmd           -> Iter.return cmd
    | Get i                   -> Iter.map (fun i -> Get i                  ) (Env.valid_t_vars env i)
    | Set (i,x)               -> Iter.map (fun i -> Set (i,x)              ) (Env.valid_t_vars env i)
    | Exchange (i,x)          -> Iter.map (fun i -> Exchange (i,x)         ) (Env.valid_t_vars env i)
    | Compare_and_set (i,x,y) -> Iter.map (fun i -> Compare_and_set (i,x,y)) (Env.valid_t_vars env i)
    | Fetch_and_add (i,x)     -> Iter.map (fun i -> Fetch_and_add (i,x)    ) (Env.valid_t_vars env i)
    | Incr i                  -> Iter.map (fun i -> Incr i                 ) (Env.valid_t_vars env i)
    | Decr i                  -> Iter.map (fun i -> Decr i                 ) (Env.valid_t_vars env i)

  type res =
    | RMake of unit
    | RGet of int
    | RSet of unit
    | RExchange of int
    | RFetch_and_add of int
    | RCompare_and_set of bool
    | RIncr of unit
    | RDecr of unit [@@deriving show { with_path = false }, eq]

  let init () = Atomic.make 0

  let run c r = match c with
    | Some t, Make i                   -> RMake (r.(t) <- Atomic.make i)
    | None, Get t                      -> RGet (Atomic.get r.(t))
    | None, Set (t,i)                  -> RSet (Atomic.set r.(t) i)
    | None, Exchange (t,i)             -> RExchange (Atomic.exchange r.(t) i)
    | None, Fetch_and_add (t,i)        -> RFetch_and_add (Atomic.fetch_and_add r.(t) i)
    | None, Compare_and_set (t,seen,v) -> RCompare_and_set (Atomic.compare_and_set r.(t) seen v)
    | None, Incr t                     -> RIncr (Atomic.incr r.(t))
    | None, Decr t                     -> RDecr (Atomic.decr r.(t))
    | _, _ -> failwith (Printf.sprintf "unexpected command: %s" (show_cmd (snd c)))

  let cleanup _ = ()
end

module AT = Lin.Make(AConf)

(*
(** A variant of the above with 3 Atomics *)
module A3Conf =
struct
  type t = int Atomic.t array

  type cmd =
    | Get of var
    | Set of var * int'
    | Exchange of var * int'
    | Compare_and_set of var * int' * int'
    | Fetch_and_add of var * int'
    | Incr of var
    | Decr of var [@@deriving qcheck, show { with_path = false }]
  and int' = int [@gen Gen.nat]
  and var  = int [@gen Gen.int_bound 2]

  let shrink_cmd = Shrink.nil

  type res =
    | RGet of int
    | RSet
    | RExchange of int
    | RFetch_and_add of int
    | RCompare_and_set of bool
    | RIncr
    | RDecr [@@deriving show { with_path = false }, eq]

  let init () = [| Atomic.make 0; Atomic.make 0; Atomic.make 0 |]

  let run c env = match c with
    | Get v                    -> RGet (Atomic.get env.(v))
    | Set (v,i)                -> (Atomic.set env.(v) i; RSet)
    | Exchange (v,i)           -> RExchange (Atomic.exchange env.(v) i)
    | Fetch_and_add (v,i)      -> RFetch_and_add (Atomic.fetch_and_add env.(v) i)
    | Compare_and_set (v,seen,nval) -> RCompare_and_set (Atomic.compare_and_set env.(v) seen nval)
    | Incr v                   -> (Atomic.incr env.(v); RIncr)
    | Decr v                   -> (Atomic.decr env.(v); RDecr)

  let cleanup _ = ()
end

module A3T = Lin.Make(A3Conf)
*)
;;
Util.set_ci_printing ()
;;
QCheck_base_runner.run_tests_main [
  AT.lin_test     `Domain ~count:1000 ~name:"Lin Atomic test with Domain";
(* A3T.lin_test    `Domain ~count:1000 ~name:"Lin Atomic3 test with Domain"; *)
]
