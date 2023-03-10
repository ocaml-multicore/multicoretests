open QCheck

(** ********************************************************************** *)
(**                   Tests of the Atomic module                           *)
(** ********************************************************************** *)
module AConf =
struct
  type t = int Atomic.t
  open Lin
  open Internal [@@alert "-internal"]
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

  let shrink_cmd _env = Shrink.nil

  let cmd_uses_var v = function
    | Make _ -> false
    | Get i
    | Set (i,_)
    | Exchange (i,_)
    | Compare_and_set (i,_,_)
    | Fetch_and_add (i,_)
    | Incr i
    | Decr i -> i=v

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

module AT_domain = Lin_domain.Make_internal(AConf) [@alert "-internal"]
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
*)
(*module A3T_domain = Lin_domain.Make_internal(A3Conf) [@alert "-internal"]*)
;;
QCheck_base_runner.run_tests_main [
  AT_domain.lin_test  ~count:1000 ~name:"Lin Atomic test with Domain";
  (*A3T_domain.lin_test ~count:1000 ~name:"Lin Atomic3 test with Domain";*)
]
