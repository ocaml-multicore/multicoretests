open QCheck

(* ********************************************************************** *)
(*                   Tests of the Atomic module                           *)
(* ********************************************************************** *)
module AConf =
struct
  type t = int Atomic.t

  type cmd =
    | Get
    | Set of int
    | Exchange of int
    | Compare_and_set of int * int
    | Fetch_and_add of int
    | Incr
    | Decr

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Get -> cst0 "Get" fmt
    | Set x -> cst1 pp_int "Set" par fmt x
    | Exchange x -> cst1 pp_int "Exchange" par fmt x
    | Compare_and_set (x, y) -> cst2 pp_int pp_int "Compare_and_set" par fmt x y
    | Fetch_and_add x -> cst1 pp_int "Fetch_and_add" par fmt x
    | Incr -> cst0 "Incr" fmt
    | Decr -> cst0 "Decr" fmt

  let show_cmd = Util.Pp.to_show pp_cmd

  let gen_cmd =
    let open QCheck.Gen in
    let int = nat in
    oneof
      [
        pure Get;
        map (fun x -> Set x) int;
        map (fun x -> Exchange x) int;
        map2 (fun x y -> Compare_and_set (x, y)) int int;
        map (fun x -> Fetch_and_add x) int;
        pure Incr;
        pure Decr;
      ]

  let shrink_cmd = Shrink.nil

  type res =
    | RGet of int
    | RSet
    | RExchange of int
    | RFetch_and_add of int
    | RCompare_and_set of bool
    | RIncr
    | RDecr

  let pp_res par fmt x =
    let open Util.Pp in
    match x with
    | RGet x -> cst1 pp_int "RGet" par fmt x
    | RSet -> cst0 "RSet" fmt
    | RExchange x -> cst1 pp_int "RExchange" par fmt x
    | RFetch_and_add x -> cst1 pp_int "RFetch_and_add" par fmt x
    | RCompare_and_set x -> cst1 pp_bool "RCompare_and_set" par fmt x
    | RIncr -> cst0 "RIncr" fmt
    | RDecr -> cst0 "RDecr" fmt

  let show_res = Util.Pp.to_show pp_res

  let equal_res x y =
    let open Util.Equal in
    match (x, y) with
    | RGet x, RGet y -> equal_int x y
    | RSet, RSet -> true
    | RExchange x, RExchange y -> equal_int x y
    | RFetch_and_add x, RFetch_and_add y -> equal_int x y
    | RCompare_and_set x, RCompare_and_set y -> equal_bool x y
    | RIncr, RIncr -> true
    | RDecr, RDecr -> true
    | _, _ -> false

  let init () = Atomic.make 0

  let run c r = match c with
    | Get                      -> RGet (Atomic.get r)
    | Set i                    -> (Atomic.set r i; RSet)
    | Exchange i               -> RExchange (Atomic.exchange r i)
    | Fetch_and_add i          -> RFetch_and_add (Atomic.fetch_and_add r i)
    | Compare_and_set (seen,v) -> RCompare_and_set (Atomic.compare_and_set r seen v)
    | Incr                     -> (Atomic.incr r; RIncr)
    | Decr                     -> (Atomic.decr r; RDecr)

  let cleanup _ = ()
end

module AT_domain = Lin_domain.Make_internal(AConf) [@alert "-internal"]

(** A variant of the above with 3 Atomics *)
module A3Conf =
struct
  type t = int Atomic.t array

  type cmd =
    | Get of var
    | Set of var * int
    | Exchange of var * int
    | Compare_and_set of var * int * int
    | Fetch_and_add of var * int
    | Incr of var
    | Decr of var
  and var = int

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Get x -> cst1 pp_int "Get" par fmt x
    | Set (x, y) -> cst2 pp_int pp_int "Set" par fmt x y
    | Exchange (x, y) -> cst2 pp_int pp_int "Exchange" par fmt x y
    | Compare_and_set (x, y, z) -> cst3 pp_int pp_int pp_int "Compare_and_set" par fmt x y z
    | Fetch_and_add (x, y) -> cst2 pp_int pp_int "Fetch_and_add" par fmt x y
    | Incr x -> cst1 pp_int "Incr" par fmt x
    | Decr x -> cst1 pp_int "Decr" par fmt x

  let show_cmd = Util.Pp.to_show pp_cmd

  let gen_cmd =
    let open QCheck.Gen in
    let var = int_bound 2 and int = nat in
    oneof
      [
        map (fun x -> Get x) var;
        map2 (fun x y -> Set (x, y)) var int;
        map2 (fun x y -> Exchange (x, y)) var int;
        map3 (fun x y z -> Compare_and_set (x, y, z)) var int int;
        map2 (fun x y -> Fetch_and_add (x, y)) var int;
        map (fun x -> Incr x) var;
        map (fun x -> Decr x) var;
      ]

  let shrink_cmd = Shrink.nil

  type res =
    | RGet of int
    | RSet
    | RExchange of int
    | RFetch_and_add of int
    | RCompare_and_set of bool
    | RIncr
    | RDecr

  let pp_res par fmt x =
    let open Util.Pp in
    match x with
    | RGet x -> cst1 pp_int "RGet" par fmt x
    | RSet -> cst0 "RSet" fmt
    | RExchange x -> cst1 pp_int "RExchange" par fmt x
    | RFetch_and_add x -> cst1 pp_int "RFetch_and_add" par fmt x
    | RCompare_and_set x -> cst1 pp_bool "RCompare_and_set" par fmt x
    | RIncr -> cst0 "RIncr" fmt
    | RDecr -> cst0 "RDecr" fmt

  let show_res = Util.Pp.to_show pp_res

  let equal_res x y =
    let open Util.Equal in
    match (x, y) with
    | RGet x, RGet y -> equal_int x y
    | RSet, RSet -> true
    | RExchange x, RExchange y -> equal_int x y
    | RFetch_and_add x, RFetch_and_add y -> equal_int x y
    | RCompare_and_set x, RCompare_and_set y -> equal_bool x y
    | RIncr, RIncr -> true
    | RDecr, RDecr -> true
    | _, _ -> false

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

module A3T_domain = Lin_domain.Make_internal(A3Conf) [@alert "-internal"]
;;
QCheck_base_runner.run_tests_main [
  AT_domain.lin_test  ~count:1000 ~name:"Lin.Internal Atomic test with Domain";
  A3T_domain.lin_test ~count:1000 ~name:"Lin.Internal Atomic3 test with Domain";
]
