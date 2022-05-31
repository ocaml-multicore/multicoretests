open QCheck
open STM

(** This is a parallel test of the buggy concurrent list CList *)

module CLConf (T : sig type t val zero : t val of_int : int -> t val to_string : t -> string end) =
struct
  let pp_t fmt t = Format.fprintf fmt "%s" (T.to_string t)
  type cmd =
    | Add_node of (T.t [@printer pp_t])
    | Member of (T.t [@printer pp_t]) [@@deriving show { with_path = false }]
  type state = T.t list
  type sut = T.t CList.conc_list Atomic.t

  let arb_cmd s =
    let int_gen = Gen.(map T.of_int nat) in
    let mem_gen =
      if s=[]
      then int_gen
      else Gen.oneof [int_gen; Gen.oneofl s]
    in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [ Gen.map (fun i -> Add_node i) int_gen;
	   Gen.map (fun i -> Member i) mem_gen; ])

  let init_state  = [ T.zero ]
  let init_sut () = CList.list_init T.zero
  let cleanup _   = ()

  let next_state c s = match c with
    | Add_node i -> i::s
    | Member _i  -> s

  let run c r = match c with
    | Add_node i -> Res (bool, CList.add_node r i)
    | Member i   -> Res (bool, CList.member r i)

  let precond _ _ = true

  let postcond c s res = match c,res with
    | Add_node _, Res ((Bool,_),v) -> v = true
    | Member i,   Res ((Bool,_),v) -> v = List.mem i s
    | _,_ -> false
end

module Int = struct
  include Stdlib.Int
  let of_int (i:int) : t = i
end

module CLT_int = STM.Make(CLConf(Int))
module CLT_int64 = STM.Make(CLConf(Int64))
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count = 1000 in
   [CLT_int.agree_test       ~count ~name:"int CList test";
    CLT_int64.agree_test     ~count ~name:"int64 CList test";
    CLT_int.neg_agree_test_par   ~count ~name:"int CList test";
    CLT_int64.neg_agree_test_par ~count ~name:"int64 CList test"])
