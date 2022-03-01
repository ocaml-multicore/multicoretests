open QCheck

(** ********************************************************************** *)
(**                   Tests of the Atomic module                           *)
(** ********************************************************************** *)
module AConf =
struct
  type t = int Atomic.t

  type cmd =
    | Get
    | Set of int'
    | Exchange of int'
    | Compare_and_set of int' * int'
    | Fetch_and_add of int'
    | Incr
    | Decr [@@deriving qcheck, show { with_path = false }]
  and int' = int [@gen Gen.nat]

  type res =
    | RGet of int
    | RSet
    | RExchange of int
    | RFetch_and_add of int
    | RCompare_and_set of bool
    | RIncr
    | RDecr [@@deriving show { with_path = false }]

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

module AT = Lin.Make(AConf)

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

  type res =
    | RGet of int
    | RSet
    | RExchange of int
    | RFetch_and_add of int
    | RCompare_and_set of bool
    | RIncr
    | RDecr [@@deriving show { with_path = false }]

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


(** ********************************************************************** *)
(**                      Tests of thread-unsafe [Hashtbl]                  *)
(** ********************************************************************** *)
module HConf =
struct
  type t = (char, int) Hashtbl.t

  type cmd =
    | Clear
    | Add of char' * int'
    | Remove of char'
    | Find of char'
    | Find_opt of char'
    | Find_all of char'
    | Replace of char' * int'
    | Mem of char'
    | Length [@@deriving qcheck, show { with_path = false }]
  and int' = int [@gen Gen.nat]
  and char' = char [@gen Gen.printable]

  type res =
    | RClear
    | RAdd
    | RRemove
    | RFind of int option
    | RFind_opt of int option
    | RFind_all of int list
    | RReplace
    | RMem of bool
    | RLength of int [@@deriving show { with_path = false }]

  let init () = Hashtbl.create ~random:false 42

  let run c h = match c with
    | Clear         -> Hashtbl.clear h; RClear
    | Add (k,v)     -> Hashtbl.add h k v; RAdd
    | Remove k      -> Hashtbl.remove h k; RRemove
    | Find k        -> RFind
                         (try Some (Hashtbl.find h k)
                          with Not_found -> None)
    | Find_opt k    -> RFind_opt (Hashtbl.find_opt h k)
    | Find_all k    -> RFind_all (Hashtbl.find_all h k)
    | Replace (k,v) -> Hashtbl.replace h k v; RReplace
    | Mem k         -> RMem (Hashtbl.mem h k)
    | Length        -> RLength (Hashtbl.length h)

  let cleanup _ = ()
end

module HT = Lin.Make(HConf)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main [
  AT.lin_test     `Domain ~count:1000 ~name:"Atomic test with domains";
  A3T.lin_test    `Domain ~count:1000 ~name:"Atomic3 test with domains";
  HT.lin_test     `Domain ~count:1000 ~name:"Hashtbl test with domains";
]
