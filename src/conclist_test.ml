open QCheck

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
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count,name = 1000,"CList test" in
   [CLT.agree_test     ~count ~name;
    CLT.agree_test_par ~count ~name;])
