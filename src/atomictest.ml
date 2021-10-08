open QCheck

module CConf =
struct
  type cmd =
    | Get
    | Set of int
    | Exchange of int
    | Compare_and_set of int * int
    | Fetch_and_add of int
    | Incr
    | Decr [@@deriving show { with_path = false }]
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

  let run_cmd c s r = match c with
    | Get                      -> Atomic.get r = s
    | Set i                    -> (Atomic.set r i; true)
    | Exchange i               -> (Atomic.exchange r i = s)
    | Fetch_and_add i          -> (Atomic.fetch_and_add r i = s)
    | Compare_and_set (seen,v) -> Atomic.compare_and_set r seen v = (s=seen)
    | Incr                     -> (Atomic.incr r; true)
    | Decr                     -> (Atomic.decr r; true)

  let precond _ _ = true
end
module CT = QCSTM.Make(CConf)
;;
QCheck_runner.run_tests ~verbose:true [CT.agree_test ~count:10_000 ~name:"ref-model agreement"]
