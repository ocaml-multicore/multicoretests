open QCheck

(** ********************************************************************** *)
(**                             Tests of [Kcas]                            *)
(** ********************************************************************** *)
module KConf =
struct
  type t = int Kcas.ref

  (* missing: equal, is_on_ref, kCAS -- mk_cas, commit (already tested as [set] *)
  type cmd =
    | Get
    | Set of int'
    | Cas of int' * int'
    | TryMapNone
    (*| TryMapSome*) (* Seq,exec cannot fail - hence not linearizable with [try_map] *)
    | MapNone
    | MapSome
    | Incr
    | Decr [@@deriving qcheck, show { with_path = false }]
  and int' = (int [@gen Gen.nat])

  let shrink_cmd = Shrink.nil

  type res =
    | RGet of int
    | RSet
    | RCas of bool
    | RTryMapNone of int cas_result
    (*| RTryMapSome of int cas_result*)
    | RMapNone of int cas_result
    | RMapSome of int cas_result
    | RIncr
    | RDecr [@@deriving show { with_path = false }, eq]
  and 'a cas_result
    = 'a Kcas.cas_result =
    | Aborted
    | Failed
    | Success of 'a [@deriving show { with_path = false }, eq]

  let init () = Kcas.ref 0

  let run c r = match c with
    | Get        -> RGet (Kcas.get r)
    | Set i      -> (Kcas.set r i; RSet)
    | Cas (i,j)  -> RCas (Kcas.cas r i j)
    | TryMapNone -> RTryMapNone (Kcas.try_map r (fun _ -> None))
    (*| TryMapSome -> RTryMapSome (Kcas.try_map r (fun i -> Some i))*)
    | MapNone    -> RMapNone (Kcas.map r (fun _ -> None))
    | MapSome    -> RMapSome (Kcas.map r (fun i -> Some i))
    | Incr       -> (Kcas.incr r; RIncr)
    | Decr       -> (Kcas.decr r; RDecr)
  let cleanup _ = ()
end

module KT = Lin.Make(KConf)


(** ********************************************************************** *)
(**                           Tests of [Kcas.W1]                           *)
(** ********************************************************************** *)
module KW1Conf =
struct
  type t = int Kcas.W1.ref

  type cmd =
    | Get
    | Set of int'
    | Cas of int' * int'
    | TryMapNone
    (*| TryMapSome*) (* Seq,exec cannot fail - hence not linearizable with [try_map] *)
    | MapNone
    | MapSome
    | Incr
    | Decr [@@deriving qcheck, show { with_path = false }]
  and int' = (int [@gen Gen.nat])

  let shrink_cmd = Shrink.nil

  type res =
    | RGet of int
    | RSet
    | RCas of bool
    | RTryMapNone of int cas_result
    (*| RTryMapSome of int cas_result*)
    | RMapNone of int cas_result
    | RMapSome of int cas_result
    | RIncr
    | RDecr [@@deriving show { with_path = false }, eq]
  and 'a cas_result
    = 'a Kcas.cas_result =
    | Aborted
    | Failed
    | Success of 'a [@deriving show { with_path = false }, eq]

  let init () = Kcas.W1.ref 0

  let run c r = match c with
    | Get        -> RGet (Kcas.W1.get r)
    | Set i      -> (Kcas.W1.set r i; RSet)
    | Cas (i,j)  -> RCas (Kcas.W1.cas r i j)
    | TryMapNone -> RTryMapNone (Kcas.W1.try_map r (fun _ -> None))
    (*| TryMapSome -> RTryMapSome (Kcas.W1.try_map r (fun i -> Some i))*)
    | MapNone    -> RMapNone (Kcas.W1.map r (fun _ -> None))
    | MapSome    -> RMapSome (Kcas.W1.map r (fun i -> Some i))
    | Incr       -> (Kcas.W1.incr r; RIncr)
    | Decr       -> (Kcas.W1.decr r; RDecr)
  let cleanup _ = ()
end

module KW1T = Lin.Make(KW1Conf)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main [
  (* Kcas tests *)
  KT.neg_lin_test `Domain ~count:1000 ~name:"Kcas test";
  KW1T.lin_test   `Domain ~count:1000 ~name:"Kcas.W1 test";
]
