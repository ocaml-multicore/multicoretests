open QCheck

(** This is a parallel test of the buggy concurrent list CList *)

module CLConf (T : sig type t val dummy : t val f : int -> t val pp : t -> string end) =
struct
  module CL = CList.Make (struct type t = T.t end)
  let pp_t fmt t = Format.fprintf fmt "%s" (T.pp t)
  type cmd =
  | Add_node of T.t [@printer pp_t]
  | Member of T.t [@printer pp_t] [@@deriving show { with_path = false }]
  type state = T.t list
  type sut = CL.conc_list Atomic.t

  let arb_cmd s =
    let int_gen = fun st -> Gen.nat st |> T.f in
    let mem_gen =
      if s=[]
      then int_gen
      else Gen.oneof [int_gen; Gen.oneofl s]
    in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [ Gen.map (fun i -> Add_node i) int_gen;
	   Gen.map (fun i -> Member i) mem_gen; ])

  let init_state  = [ T.dummy ]
  let init_sut () = CL.list_init T.dummy
  let cleanup _   = ()

  let next_state c s = match c with
    | Add_node i -> i::s
    | Member _i  -> s

  type res = RAdd_node of bool | RMember of bool [@@deriving show { with_path = false }]

  let run c r = match c with
    | Add_node i -> RAdd_node (CL.add_node r i)
    | Member i   -> RMember (CL.member r i)

  let precond _ _ = true

  let postcond c s res = match c,res with
    | Add_node _, RAdd_node v -> v = true
    | Member i,   RMember v   -> v = List.mem i s
    | _,_ -> false
end

module T_int = struct
  type t = int
  let dummy = 0
  let f i = i
  let pp = Int.to_string
end

module T_int64 = struct
  type t = int64
  let dummy = Int64.zero
  let f = Int64.of_int
  let pp = Int64.to_string
end

module CLT_int = STM.Make(CLConf(T_int))
module CLT_int64 = STM.Make(CLConf(T_int64))
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count,name = 1000,"CList test" in
   [CLT_int.agree_test     ~count ~name;
    CLT_int64.agree_test ~count ~name;
    CLT_int.agree_test ~count ~name;
    CLT_int64.agree_test_par ~count ~name;])
