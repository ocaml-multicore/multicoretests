open QCheck

(** a simple concurrent list - from Sadiq *)
module CList =
struct
  type conc_list = { value: int; next: conc_list option }

  let rec add_node list_head n =
    (* try to add a new node to head *)
    let old_head = Atomic.get list_head in
    let new_node = { value = n ; next = (Some old_head) } in
    (* introduce bug *)
    if Atomic.get list_head = old_head then begin
        Atomic.set list_head new_node;
        true
      end
    else
      add_node list_head n

  let list_init i = Atomic.make { value = i ; next = None }

  let member list_head n =
    let rec check_from_node node =
      match (node.value, node.next) with
      | (v, _) when v = n -> true
      | (_, None) -> false
      | (_ , Some(next_node)) ->
         check_from_node next_node
    in
    (* try to find the node *)
    check_from_node (Atomic.get list_head)

  let add_and_check list_head n () =
    assert(add_node list_head n);
    assert(member list_head n)
end


(** This is a parallel test of the buggy concurrent list CList *)

module CLConf =
struct
  type cmd = Add_node of int | Member of int [@@deriving show { with_path = false }]
  type state = int list
  type sut = CList.conc_list Atomic.t

  let arb_cmd s =
    let int_gen = Gen.nat in
    let mem_gen =
      if s=[]
      then int_gen
      else Gen.oneof [int_gen; Gen.oneofl s]
    in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [ Gen.map (fun i -> Add_node i) int_gen;
	   Gen.map (fun i -> Member i) mem_gen; ])

  let init_state  = [0]
  let init_sut () = CList.list_init 0
  let cleanup _   = ()

  let next_state c s = match c with
    | Add_node i -> i::s
    | Member _i  -> s

  type res = RAdd_node of bool | RMember of bool [@@deriving show { with_path = false }]

  let run c r = match c with
    | Add_node i -> RAdd_node (CList.add_node r i)
    | Member i   -> RMember (CList.member r i)

  let precond _ _ = true

  let postcond c s res = match c,res with
    | Add_node _, RAdd_node v -> v = true
    | Member i,   RMember v   -> v = List.mem i s
    | _,_ -> false
end

module CLT = STM.Make(CLConf)

let agree_test_par ~count ~name =
  let seq_len,par_len = 20,15 in
  Non_det.Test.make ~count ~name
    (CLT.arb_cmds_par seq_len par_len) CLT.agree_prop_par

let agree_test_pardomlib ~count ~name =
  let seq_len,par_len = 20,15 in
  Non_det.Test.make ~count ~name
    (CLT.arb_cmds_par seq_len par_len) CLT.agree_prop_pardomlib

let agree_test_par_comb ~count ~name = (* a combination of repeat and Non_det *)
  let seq_len,par_len = 20,15 in
  let rep_count = 15 in
  Non_det.Test.make ~repeat:15 ~count ~name
    (CLT.arb_cmds_par seq_len par_len)
    (STM.repeat rep_count CLT.agree_prop_par) (* 15 times each, then 15 * 15 times when shrinking *)

;;
Non_det.QCheck_runner.run_tests_main [
    CLT.agree_test           ~count:1000 ~name:"sequential test of CList";
    CLT.agree_test_par       ~count:1000 ~name:"parallel test of CList (w/repeat)";
        agree_test_par       ~count:1000 ~name:"parallel test of CList (w/non_det module)";
    CLT.agree_test_pardomlib ~count:1000 ~name:"parallel test of CList (w/Domainslib.Task and repeat)";
        agree_test_pardomlib ~count:1000 ~name:"parallel test of CList (w/Domainslib.Task and non_det module)";
        agree_test_par_comb  ~count:1000 ~name:"parallel test of CList (w/repeat and Non_det combined)";
  ]
