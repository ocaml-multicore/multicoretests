open QCheck

(** This is a parallel test of refs *)

module Sut =
  struct
    let sut = ref 0
    let get () = !sut
    let set i  = sut:=i
    let add i  = let old = !sut in sut:=i + old (* buggy: not atomic *)
    let incr () = incr sut     (* buggy: not guaranteed to be atomic *)
    let decr () = decr sut     (* buggy: not guaranteed to be atomic *)
end

module RConf =
struct
  type cmd =
    | Get
    | Set of int
    | Add of int
    | Incr
    | Decr [@@deriving show { with_path = false }]
  type state = int
  type sut = int ref

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [Gen.return Get;
	  Gen.map (fun i -> Set i) int_gen;
	  Gen.map (fun i -> Add i) int_gen;
          Gen.return Incr;
	  Gen.return Decr;
         ])

  let init_state  = 0
  let init_sut () = Sut.sut
  let cleanup _   = Sut.set 0

  let next_state c s = match c with
    | Get   -> s
    | Set i -> i (*if i<>1213 then i else s*) (* an artificial fault *)
    | Add i -> s+i
    | Incr  -> s+1
    | Decr  -> s-1

  let precond _ _ = true

  type res = RGet of int | RSet | RAdd | RIncr | RDecr [@@deriving show { with_path = false }]

  let run c _r =
    match c with
    | Get   -> RGet (Sut.get ())
    | Set i -> (Sut.set i; RSet)
    | Add i -> (Sut.add i; RAdd)
    | Incr  -> (Sut.incr (); RIncr)
    | Decr  -> (Sut.decr (); RDecr)

  let postcond c s res = match c,res with
    | Get, RGet v -> v = s (*&& v<>42*) (*an injected bug*)
    | Set _, RSet -> true
    | Add _, RAdd -> true
    | Incr, RIncr -> true
    | Decr, RDecr -> true
    | _,_ -> false
end


module RT = STM.Make(RConf)

module RConfGC = STM.AddGC(RConf)
module RTGC = STM.Make(RConfGC)

let agree_prop_pargc =
  (fun (seq_pref,cmds1,cmds2) ->
    assume (RTGC.cmds_ok RConfGC.init_state (seq_pref@cmds1));
    assume (RTGC.cmds_ok RConfGC.init_state (seq_pref@cmds2));
    let sut = RConfGC.init_sut () in
    let res = RTGC.interp_sut_res sut seq_pref in
    let dom1 = Domain.spawn (fun () -> (*Gc.minor(); Gc.minor();*) RTGC.interp_sut_res sut cmds1) in
    let dom2 = Domain.spawn (fun () -> RTGC.interp_sut_res sut cmds2) in
    let obs1 = Domain.join dom1 in
    let obs2 = Domain.join dom2 in
    let res  = RTGC.check_obs res obs1 obs2 RConfGC.init_state in
    let ()   = RConf.cleanup sut in
    res)

let agree_test_pargc ~count ~name =
  let rep_count = 50 in
  let seq_len,par_len = 20,15 in
  Test.make ~count ~name
    (RTGC.arb_cmds_par seq_len par_len)
    (STM.repeat rep_count agree_prop_pargc)
;;
QCheck_runner.run_tests_main
  ((RT.agree_test_suite ~count:1000 ~name:"global ref test")
   @
   [agree_test_pargc ~count:1000 ~name:"parallel global ref test (w/repeat and AddGC functor)"])
