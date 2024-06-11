open Domain
open QCheck
open STM

(** parallel STM tests of Domain.DLS *)

module DLSConf =
struct
  let length = 4

  type index = int
  type cmd =
    | Get of index
    | Set of index * int

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Get i -> cst1 pp_int "Get" par fmt i
    | Set (i,x) -> cst2 pp_int pp_int "Set" par fmt i x

  let show_cmd = Util.Pp.to_show pp_cmd

  type state = int list
  type sut   = int Domain.DLS.key list

  let arb_cmd _s =
    let index = Gen.int_bound (length-1) in
    let int_gen = Gen.small_nat in
    QCheck.make ~print:show_cmd
      Gen.(oneof
             [ map (fun i -> Get i) index;
               map2 (fun i x -> Set (i,x)) index int_gen;
             ])

  let init_state  = List.init length (fun i -> i)

  let next_state n s = match n with
    | Get _     -> s
    | Set (i,n) -> List.mapi (fun j x -> if i=j then n else x) s

  let init_sut () = List.init length (fun i -> DLS.new_key ~split_from_parent:(fun x -> x) (fun () -> i))

  let cleanup _   = ()

  let precond n _s = match n with
    | _ -> true

  let run n t = match n with
    | Get i     -> Res (STM.int, Domain.DLS.get (List.nth t i))
    | Set (i,x) -> Res (unit, Domain.DLS.set (List.nth t i) x)

  let postcond n (s:int list) res = match n, res with
    | Get i, Res ((Int,_), r) -> (List.nth s i) = r
    | Set _, Res ((Unit,_), ()) -> true
    | _, _ -> false
end

module DLS_STM_seq = STM_sequential.Make(DLSConf)
module DLS_STM_dom = STM_domain.Make(DLSConf)

(* Run seq. property in a child domain to have a clean DLS for each iteration *)
let agree_prop cs = Domain.spawn (fun () -> DLS_STM_seq.agree_prop cs) |> Domain.join

(* Run domain property in a child domain to have a clean DLS for each iteration *)
let agree_prop_par t = Domain.spawn (fun () -> DLS_STM_dom.agree_prop_par t) |> Domain.join

(* Run stress property in a child domain to have a clean DLS for each iteration *)
let stress_prop_par t = Domain.spawn (fun () -> DLS_STM_dom.stress_prop_par t) |> Domain.join

let agree_test ~count ~name =
  Test.make ~name ~count (DLS_STM_seq.arb_cmds DLSConf.init_state) agree_prop

let neg_agree_test_par ~count ~name =
  let seq_len,par_len = 20,12 in
  Test.make_neg ~retries:10 ~count ~name
    (DLS_STM_dom.arb_cmds_triple seq_len par_len)
    (fun triple ->
       assume (DLS_STM_dom.all_interleavings_ok triple);
       agree_prop_par triple) (* just repeat 1 * 10 times when shrinking *)

let stress_test_par ~count ~name =
  let seq_len,par_len = 20,12 in
  Test.make ~retries:10 ~count ~name
    (DLS_STM_dom.arb_cmds_triple seq_len par_len)
    (fun triple ->
       assume (DLS_STM_dom.all_interleavings_ok triple);
       stress_prop_par triple) (* just repeat 1 * 10 times when shrinking *)
;;
;;
QCheck_base_runner.run_tests_main [
  agree_test         ~count:1000 ~name:"STM Domain.DLS test sequential";
  neg_agree_test_par ~count:1000 ~name:"STM Domain.DLS test parallel";
  stress_test_par    ~count:1000 ~name:"STM Domain.DLS stress test parallel";
]
