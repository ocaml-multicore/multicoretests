open QCheck
open STM

(** This is a parallel test of the Atomic module *)

module CConf =
struct
  type _ cmd =
    | Get : int cmd
    | Set : int -> unit cmd
    | Exchange : int -> int cmd
    | Compare_and_set : int * int -> bool cmd
    | Fetch_and_add : int -> int cmd
    | Incr : unit cmd
    | Decr : unit cmd

  let pp_cmd : type r. bool -> Format.formatter -> r cmd -> unit = fun par fmt x ->
    let open Util.Pp in
    match x with
    | Get -> cst0 "Get" fmt
    | Set x -> cst1 pp_int "Set" par fmt x
    | Exchange x -> cst1 pp_int "Exchange" par fmt x
    | Compare_and_set (x, y) -> cst2 pp_int pp_int "Compare_and_set" par fmt x y
    | Fetch_and_add x -> cst1 pp_int "Fetch_and_add" par fmt x
    | Incr -> cst0 "Incr" fmt
    | Decr -> cst0 "Decr" fmt

  let show_cmd : type r. r cmd -> string = fun cmd ->
    Util.Pp.to_show pp_cmd cmd

  type state = int
  type sut = int Atomic.t

  type packed_cmd = Pack_cmd : 'r cmd -> packed_cmd

  let show_packed_cmd (Pack_cmd c) = show_cmd c

  let arb_cmd s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_packed_cmd
      (Gen.oneof
         [Gen.return (Pack_cmd Get);
	  Gen.map (fun i -> Pack_cmd (Set i)) int_gen;
	  Gen.map (fun i -> Pack_cmd (Exchange i)) int_gen;
	  Gen.map (fun i -> Pack_cmd (Fetch_and_add i)) int_gen;
	  Gen.map2 (fun seen v -> Pack_cmd (Compare_and_set (seen,v))) (Gen.oneof [Gen.return s; int_gen]) int_gen;
          Gen.return (Pack_cmd Incr);
	  Gen.return (Pack_cmd Decr);
         ])

  let init_state  = 0
  let init_sut () = Atomic.make 0
  let cleanup _   = ()

  let next_state : type r. r cmd -> state -> state = fun c s ->
    match c with
    | Get                      -> s
    | Set i                    -> i (*if i<>1213 then i else s*) (* an artificial fault *)
    | Exchange i               -> i
    | Fetch_and_add i          -> s+i
    | Compare_and_set (seen,v) -> if s=seen then v else s
    | Incr                     -> s+1
    | Decr                     -> s-1

  let precond _ _ = true

  let run : type r. r cmd -> sut -> r res  = fun c r ->
    match c with
    | Get                      -> int,  Atomic.get r
    | Set i                    -> unit, Atomic.set r i
    | Exchange i               -> int,  Atomic.exchange r i
    | Fetch_and_add i          -> int,  Atomic.fetch_and_add r i
    | Compare_and_set (seen,v) -> bool, Atomic.compare_and_set r seen v
    | Incr                     -> unit, Atomic.incr r
    | Decr                     -> unit, Atomic.decr r

  let postcond : type r. r cmd -> state -> r -> bool = fun c s res ->
    match c,res with
    | Get,             v  -> v = s (*&& v<>42*) (*an injected bug*)
    | Set _,           _  -> true
    | Exchange _,      v  -> v = s
    | Fetch_and_add _, v  -> v = s
    | Compare_and_set (seen,_), b -> b = (s=seen)
    | Incr,            () -> true
    | Decr,            () -> true
end

module AT_seq = STM_sequential.Make(CConf)
module AT_dom = STM_domain.Make(CConf)
;;
QCheck_base_runner.run_tests_main
  (let count = 250 in
   [AT_seq.agree_test     ~count ~name:"STM Atomic test sequential";
    AT_dom.agree_test_par ~count ~name:"STM Atomic test parallel";])
