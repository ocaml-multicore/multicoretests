open QCheck
open STM

(** This is a parallel test of the Atomic module *)

module CConf =
struct
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

  type state = int
  type sut = int Atomic.t

  let arb_cmd s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [Gen.return Get;
	  Gen.map (fun i -> Set i) int_gen;
	  Gen.map (fun i -> Exchange i) int_gen;
	  Gen.map (fun i -> Fetch_and_add i) int_gen;
	  Gen.map2 (fun seen v -> Compare_and_set (seen,v)) (Gen.oneof [Gen.return s; int_gen]) int_gen;
          Gen.return Incr;
	  Gen.return Decr;
         ])

  let init_state  = 0
  let init_sut () = Atomic.make 0
  let cleanup _   = ()

  let next_state c s = match c with
    | Get                      -> s
    | Set i                    -> i (*if i<>1213 then i else s*) (* an artificial fault *)
    | Exchange i               -> i
    | Fetch_and_add i          -> s+i
    | Compare_and_set (seen,v) -> if s=seen then v else s
    | Incr                     -> s+1
    | Decr                     -> s-1

  let precond _ _ = true

  let run c r =
    match c with
    | Get                      -> Res (int,  Atomic.get r)
    | Set i                    -> Res (unit, Atomic.set r i)
    | Exchange i               -> Res (int,  Atomic.exchange r i)
    | Fetch_and_add i          -> Res (int,  Atomic.fetch_and_add r i)
    | Compare_and_set (seen,v) -> Res (bool, Atomic.compare_and_set r seen v)
    | Incr                     -> Res (unit, Atomic.incr r)
    | Decr                     -> Res (unit, Atomic.decr r)

  let postcond c (s : state) res =
    match c,res with
    | Get,             Res ((Int,_),v)  -> v = s (*&& v<>42*) (*an injected bug*)
    | Set _,           Res ((Unit,_),_) -> true
    | Exchange _,      Res ((Int,_),v)  -> v = s
    | Fetch_and_add _, Res ((Int,_),v)  -> v = s
    | Compare_and_set (seen,_), Res ((Bool,_),b) -> b = (s=seen)
    | Incr,            Res ((Unit,_),_) -> true
    | Decr,            Res ((Unit,_),_) -> true
    | _,_ -> false
end

module AT_seq = STM_sequential.Make(CConf)
module AT_dom = STM_domain.Make(CConf)
;;
QCheck_base_runner.run_tests_main
  (let count = 250 in
   [AT_seq.agree_test     ~count ~name:"STM Atomic test sequential";
    AT_dom.agree_test_par ~count ~name:"STM Atomic test parallel";])
