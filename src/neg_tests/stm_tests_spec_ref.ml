open QCheck
open STM

(** This is a parallel test of refs *)

module Sut_int =
  struct
    let init () = ref 0
    let get r = !r
    let set r i = r:=i
    let add r i = let old = !r in r:=i + old (* buggy: not atomic *)
    let incr r = incr r                      (* buggy: not atomic *)
    let decr r = decr r                      (* buggy: not atomic *)
end

module Sut_int64 =
  struct
    let init () = ref Int64.zero
    let get r = !r
    let set r i = r:=i
    let add r i = let old = !r in r:= Int64.add i old (* buggy: not atomic *)
    let incr r = add r Int64.one                      (* buggy: not atomic *)
    let decr r = add r Int64.minus_one                (* buggy: not atomic *)
end

module RConf_int =
struct
  type sut = int ref
  type state = int
  type cmd =
    | Get
    | Set of int
    | Add of int
    | Incr
    | Decr

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Get -> cst0 "Get" fmt
    | Set x -> cst1 pp_int "Set" par fmt x
    | Add x -> cst1 pp_int "Add" par fmt x
    | Incr -> cst0 "Incr" fmt
    | Decr -> cst0 "Decr" fmt

  let show_cmd = Util.Pp.to_show pp_cmd

  let arb_cmd _s =
    let int_gen = Gen.nat in
    let shrink_cmd c = match c with
      | Set i -> Iter.map (fun i -> Set i) (Shrink.int i)
      | Add i -> Iter.map (fun i -> Add i) (Shrink.int i)
      | _ -> Iter.empty in
    QCheck.make ~print:show_cmd ~shrink:shrink_cmd
      (Gen.oneof
         [Gen.return Get;
	  Gen.map (fun i -> Set i) int_gen;
	  Gen.map (fun i -> Add i) int_gen;
          Gen.return Incr;
	  Gen.return Decr;
         ])

  let init_state  = 0
  let init_sut () = Sut_int.init ()
  let cleanup _   = ()

  let next_state c s = match c with
    | Get   -> s
    | Set i -> i (*if i<>1213 then i else s*) (* an artificial fault *)
    | Add i -> s+i
    | Incr  -> s+1
    | Decr  -> s-1

  let precond _ _ = true

  let run c r =
    match c with
    | Get   -> Res (int,  Sut_int.get r)
    | Set i -> Res (unit, Sut_int.set r i)
    | Add i -> Res (unit, Sut_int.add r i)
    | Incr  -> Res (unit, Sut_int.incr r)
    | Decr  -> Res (unit, Sut_int.decr r)

  let postcond c (s : state) res =
    match c,res with
    | Get,   Res ((Int,_),v)  -> v = s (*&& v<>42*) (*an injected bug*)
    | Set _, Res ((Unit,_),_) -> true
    | Add _, Res ((Unit,_),_) -> true
    | Incr,  Res ((Unit,_),_) -> true
    | Decr,  Res ((Unit,_),_) -> true
    | _,_ -> false
end

module RConf_int64 =
struct
  type sut = int64 ref
  type state = int64
  type cmd =
    | Get
    | Set of int64
    | Add of int64
    | Incr
    | Decr

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Get -> cst0 "Get" fmt
    | Set x -> cst1 pp_int64 "Set" par fmt x
    | Add x -> cst1 pp_int64 "Add" par fmt x
    | Incr -> cst0 "Incr" fmt
    | Decr -> cst0 "Decr" fmt

  let show_cmd = Util.Pp.to_show pp_cmd

  let arb_cmd _s =
    let int64_gen = Gen.(map Int64.of_int nat) in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [Gen.return Get;
	  Gen.map (fun i -> Set i) int64_gen;
	  Gen.map (fun i -> Add i) int64_gen;
          Gen.return Incr;
	  Gen.return Decr;
         ])

  let init_state  = 0L
  let init_sut () = Sut_int64.init ()
  let cleanup _   = ()

  let next_state c s = match c with
    | Get   -> s
    | Set i -> i (*if i<>1213 then i else s*) (* an artificial fault *)
    | Add i -> Int64.add s i
    | Incr  -> Int64.succ s
    | Decr  -> Int64.pred s

  let precond _ _ = true

  let run c r =
    match c with
    | Get   -> Res (int64, Sut_int64.get r)
    | Set i -> Res (unit, Sut_int64.set r i)
    | Add i -> Res (unit, Sut_int64.add r i)
    | Incr  -> Res (unit, Sut_int64.incr r)
    | Decr  -> Res (unit, Sut_int64.decr r)

  let postcond c s res =
    match c,res with
    | Get,   Res ((Int64,_),(v:int64)) -> v = s (*&& v<>42L*) (*an injected bug*)
    | Set _, Res ((Unit,_),_) -> true
    | Add _, Res ((Unit,_),_) -> true
    | Incr,  Res ((Unit,_),_) -> true
    | Decr,  Res ((Unit,_),_) -> true
    | _,_ -> false
end
