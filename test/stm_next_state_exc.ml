open QCheck
open STM

exception Random_next_state_failure

(** This is a variant of refs to test for exceptions in next_state *)

module RConf =
struct

  type cmd = Get | Set of int | Add of int

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Get -> cst0 "Get" fmt
    | Set x -> cst1 pp_int "Set" par fmt x
    | Add x -> cst1 pp_int "Add" par fmt x

  let show_cmd = Util.Pp.to_show pp_cmd

  let gen_cmd =
    let int_gen = Gen.nat in
      (Gen.oneof
         [Gen.return Get;
	  Gen.map (fun i -> Set i) int_gen;
	  Gen.map (fun i -> Add i) int_gen;
         ])
  let arb_cmd _ = make ~print:show_cmd gen_cmd

  type state = int

  let init_state = 0

  let next_state c s = match c with
    | Get -> s
    | Set i -> i
    | Add i -> if i>70 then raise Random_next_state_failure; s+i

  type sut = int ref

  let init_sut () = ref 0

  let cleanup _ = ()

  let run c r = match c with
    | Get   -> Res (int, !r)
    | Set i -> Res (unit, (r:=i))
    | Add i -> Res (unit, let old = !r in r := i + old) (* buggy: not atomic *)

  let precond _ _ = true

  let postcond c (s:state) res = match c,res with
    | Get,   Res ((Int,_),r) -> r = s
    | Set _, Res ((Unit,_),_)
    | Add _, Res ((Unit,_),_) -> true
    | _,_ -> false
end

module RT_int = STM.Internal.Make(RConf)[@alert "-internal"]
module RT_seq = STM_sequential.Make(RConf)
module RT_dom = STM_domain.Make(RConf)

let () = QCheck_base_runner.set_seed 301717275
let _ =
  QCheck_base_runner.run_tests ~verbose:true
    [RT_int.consistency_test ~count:1000 ~name:"STM test exception during next_state consistency"]
let () = (* raises Test_error not handled by neg_agree_test so handled explicitly *)
  let name = "STM test exception during next_state sequential" in
  try
    Test.check_exn (RT_seq.agree_test ~count:1000 ~name);
    Printf.printf "%s unexpectedly succeeded\n%!" name;
  with Test.Test_error (err_name,_,Random_next_state_failure,_) ->
    assert (err_name = name);
    Printf.printf "%s failed with Random_next_state_failure as expected\n%!" name
let () = (* raises Test_error not handled by neg_agree_test so handled explicitly *)
  let name = "STM test exception during next_state parallel" in
  try
    Test.check_exn (RT_dom.agree_test_par ~count:1000 ~name);
    Printf.printf "%s unexpectedly succeeded\n%!" name;
  with Test.Test_error (err_name,_,Random_next_state_failure,_) ->
    assert (err_name = name);
    Printf.printf "%s failed with Random_next_state_failure as expected\n%!" name
