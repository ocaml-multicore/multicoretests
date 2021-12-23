open QCheck

(** This is a parallel test of [lf_list] from the lockfree library *)

module LFLConf = (* for plain list operations *)
struct
  type cmd = Push of int | Pop | Is_empty | To_string [@@deriving show { with_path = false }]
  type state = int list
  type sut = int Lockfree.List.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      Gen.(oneof [
               map (fun i -> Push i) int_gen;
               return Pop;
               return Is_empty;
               return To_string;
      ])

  let init_state  = []
  let init_sut () = Lockfree.List.create ()
  let cleanup _   = ()

  let next_state c s = match c with
    | Push i    -> i::s
    | Pop       -> (match s with [] -> [] | _::s' -> s')
    | Is_empty  -> s
    | To_string -> s

  type res =
    | RPush
    | RPop of int option
    | RIs_empty of bool
    | RTo_string of string [@@deriving show { with_path = false }]

  let run c l = match c with
    | Push i    -> (Lockfree.List.push l i; RPush)
    | Pop       -> RPop (Lockfree.List.pop l)
    | Is_empty  -> RIs_empty (Lockfree.List.is_empty l)
    | To_string -> RTo_string (Lockfree.List.to_string l string_of_int)

  let precond _ _ = true

  let postcond c s res = match c,res with
    | Push _,    RPush         -> true
    | Pop,       RPop opt      -> opt = (try Some (List.hd s) with Failure _ -> None)
    | Is_empty,  RIs_empty r   -> r = (s=[])
    | To_string, RTo_string sl ->
       sl = "[" ^ (String.concat " ; " (List.map Print.int s)) ^ "]\n"
    | _,_ -> false
end

module LFOLConf = (* for ordered list operations *)
struct
  type cmd = Mem of int | Find of int | SInsert of int [@@deriving show { with_path = false }]
  type state = int list
  type sut = int Lockfree.List.t

  let arb_cmd s =
    let int_gen =
      if s=[]
      then Gen.nat
      else Gen.oneof [ Gen.oneofl s; Gen.nat ]
    in
    QCheck.make ~print:show_cmd
      Gen.(oneof [
               map (fun i -> Mem i) int_gen;
               map (fun i -> Find i) int_gen;
               map (fun i -> SInsert i) int_gen;
      ])

  let init_state  = []
  let init_sut () = Lockfree.List.create ()
  let cleanup _   = ()

  let next_state c s = match c with
    | Mem _     -> s
    | Find _    -> s
    | SInsert i -> if List.mem i s
                   then s
                   else List.sort Int.compare (i::s)

  type res =
    | RMem of bool
    | RFind of int option
    | RSInsert of bool (*sut*) [@@deriving show { with_path = false }]

  let run c l = match c with
    | Mem i     -> RMem (Lockfree.List.mem l i Int.compare)
    | Find i    -> RFind (Lockfree.List.find l i Int.compare)
    | SInsert i -> let b,_l' = (Lockfree.List.sinsert l i Int.compare) in
                   RSInsert b (*FIXME l' ignored *)

  let precond _ _ = true

  let postcond c s res = match c,res with
    | Mem i,     RMem b     -> b = (List.mem i s)
    | Find i,    RFind i'   -> i' = (if List.mem i s then Some i else None)
    | SInsert i, RSInsert b -> b = not (List.mem i s)
    | _,_ -> false
end

module LFLT = STM.Make(LFLConf)
module LFOLT = STM.Make(LFOLConf)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count,name = 1000,"lf_list test" in
   [LFLT.agree_test     ~count ~name;
    LFLT.agree_test_par ~count ~name;])
   @
  (let count,name = 1000,"ordered lf_list test" in
   [LFOLT.agree_test     ~count ~name;
    LFOLT.agree_test_par ~count ~name;])
