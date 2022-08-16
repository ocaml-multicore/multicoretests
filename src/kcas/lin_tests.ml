open QCheck

(** ********************************************************************** *)
(**                             Tests of [Kcas]                            *)
(** ********************************************************************** *)
module KConf =
struct
  type t = int Kcas.ref

  open Lin
  (* missing: equal, is_on_ref, kCAS -- mk_cas, commit (already tested as [set] *)
  type cmd =
    | Get of Var.t
    | Set of Var.t * int
    | Cas of Var.t * int * int
    | TryMapNone of Var.t
    (*| TryMapSome of Var.t *) (* Seq,exec cannot fail - hence not linearizable with [try_map] *)
    | MapNone of Var.t
    | MapSome of Var.t
    | Incr of Var.t
    | Decr of Var.t [@@deriving show { with_path = false }]

  let gen_int = Gen.nat
  let gen_cmd gen_var =
    Gen.(oneof [
        map  (fun t     -> None, Get t) gen_var;
        map2 (fun t i   -> None, Set (t,i)) gen_var gen_int;
        map3 (fun t i j -> None, Cas (t,i,j)) gen_var gen_int gen_int;
        map  (fun t     -> None, TryMapNone t) gen_var;
      (*map  (fun t     -> None, TryMapSone t) gen_var;*)
        map  (fun t     -> None, MapNone t) gen_var;
        map  (fun t     -> None, MapSome t) gen_var;
        map  (fun t     -> None, Incr t) gen_var;
        map  (fun t     -> None, Decr t) gen_var;
      ])

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
    | None, Get t        -> RGet (Kcas.get r.(t))
    | None, Set (t,i)    -> (Kcas.set r.(t) i; RSet)
    | None, Cas (t,i,j)  -> RCas (Kcas.cas r.(t) i j)
    | None, TryMapNone t -> RTryMapNone (Kcas.try_map r.(t) (fun _ -> None))
  (*| None, TryMapSome t -> RTryMapSome (Kcas.try_map r.(t) (fun i -> Some i))*)
    | None, MapNone t    -> RMapNone (Kcas.map r.(t) (fun _ -> None))
    | None, MapSome t    -> RMapSome (Kcas.map r.(t) (fun i -> Some i))
    | None, Incr t       -> (Kcas.incr r.(t); RIncr)
    | None, Decr t       -> (Kcas.decr r.(t); RDecr)
    | _, _ -> failwith (Printf.sprintf "unexpected command: %s" (show_cmd (snd c)))
  let cleanup _ = ()
end

module KT = Lin.Make(KConf)


(** ********************************************************************** *)
(**                           Tests of [Kcas.W1]                           *)
(** ********************************************************************** *)
module KW1Conf =
struct
  type t = int Kcas.W1.ref

  open Lin
  type cmd =
    | Get of Var.t
    | Set of Var.t * int
    | Cas of Var.t * int * int
    | TryMapNone of Var.t
  (*| TryMapSome of Var.t *) (* Seq,exec cannot fail - hence not linearizable with [try_map] *)
    | MapNone of Var.t
    | MapSome of Var.t
    | Incr of Var.t
    | Decr of Var.t [@@deriving show { with_path = false }]
  let gen_int = Gen.nat
  let gen_cmd gen_var =
    Gen.(oneof [
        map  (fun t -> None,Get t) gen_var;
        map2 (fun t i -> None,Set (t,i)) gen_var gen_int;
        map3 (fun t i j -> None,Cas (t,i,j)) gen_var gen_int gen_int;
        map  (fun t -> None,TryMapNone t) gen_var;
      (*map  (fun t -> None,TryMapSome t) gen_var;*)
        map  (fun t -> None,MapNone t) gen_var;
        map  (fun t -> None,MapSome t) gen_var;
        map  (fun t -> None,Incr t) gen_var;
        map  (fun t -> None,Decr t) gen_var;
      ])

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
    | None,Get t        -> RGet (Kcas.W1.get r.(t))
    | None,Set (t,i)    -> (Kcas.W1.set r.(t) i; RSet)
    | None,Cas (t,i,j)  -> RCas (Kcas.W1.cas r.(t) i j)
    | None,TryMapNone t -> RTryMapNone (Kcas.W1.try_map r.(t) (fun _ -> None))
  (*| None,TryMapSome t -> RTryMapSome (Kcas.W1.try_map r.(t) (fun i -> Some i))*)
    | None,MapNone t    -> RMapNone (Kcas.W1.map r.(t) (fun _ -> None))
    | None,MapSome t    -> RMapSome (Kcas.W1.map r.(t) (fun i -> Some i))
    | None,Incr t       -> (Kcas.W1.incr r.(t); RIncr)
    | None,Decr t       -> (Kcas.W1.decr r.(t); RDecr)
    | _, _ -> failwith (Printf.sprintf "unexpected command: %s" (show_cmd (snd c)))
  let cleanup _ = ()
end

module KW1T = Lin.Make(KW1Conf)
;;
Util.set_ci_printing ()
;;
QCheck_base_runner.run_tests_main [
  (* Kcas tests *)
  KT.neg_lin_test `Domain ~count:1000 ~name:"Lin Kcas test with Domain";
  KW1T.lin_test   `Domain ~count:1000 ~name:"Lin Kcas.W1 test with Domain";
]
