open QCheck
open STM

(** A parameterized STM specification of the buggy concurrent list CList *)

module CLConf
    (CList : sig
       type 'a conc_list
       val list_init : 'a -> 'a conc_list Atomic.t
       val add_node: 'a conc_list Atomic.t -> 'a -> bool
       val member : 'a conc_list Atomic.t -> 'a -> bool
     end)
    (T : sig type t val zero : t val of_int : int -> t val to_string : t -> string end) =
struct
  type cmd =
    | Add_node of T.t
    | Member of T.t

  let pp_cmd par fmt x =
    let open Util.Pp in
    let pp_t = of_show T.to_string in
    match x with
    | Add_node x -> cst1 pp_t "Add_node" par fmt x
    | Member x -> cst1 pp_t "Member" par fmt x

  let show_cmd = Util.Pp.to_show pp_cmd

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
