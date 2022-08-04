(** Sequential tests of ws_deque *)

open QCheck
open STM

module Ws_deque = Lockfree.Ws_deque

module WSDConf =
struct
  type cmd =
    | Push of int  (* use int for now *)
    | Pop
    | Steal [@@deriving show { with_path = false }]
  type state = int list
  type sut = int Ws_deque.M.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [Gen.map (fun i -> Push i) int_gen;
          Gen.return Pop;
          (*Gen.return Steal;*) (* No point in stealing from yourself :-D *)
         ])
  let stealer_cmd _s =
    QCheck.make ~print:show_cmd (Gen.return Steal)

  let init_state  = []
  let init_sut () = Ws_deque.M.create ()
  let cleanup _   = ()

  let next_state c s = match c with
    | Push i   -> i::s (*if i<>1213 then i::s else s*) (* an artificial fault *)
    | Pop      -> (match s with
        | []    -> s
        | _::s' -> s')
    | Steal    -> (match List.rev s with
        | []    -> s
        | _::s' -> List.rev s')

  let precond _ _ = true

  let run c d = match c with
    | Push i   -> Res (unit, Ws_deque.M.push d i)
    | Pop      -> Res (result int exn, protect Ws_deque.M.pop d)
    | Steal    -> Res (result int exn, protect Ws_deque.M.steal d)

  let postcond c (s : state) res = match c,res with
    | Push _, Res ((Unit,_),_) -> true
    | Pop,    Res ((Result (Int,Exn),_),res) ->
        (match s with
         | []   -> res = Error Exit
         | j::_ -> res = Ok j)
    | Steal,  Res ((Result (Int,Exn),_),res) ->
        (match List.rev s with
         | []   -> Result.is_error res
         | j::_ -> res = Ok j)
    | _,_ -> false
end

module WSDT = STM.Make(WSDConf) ;;

QCheck_runner.run_tests_main
  (let count = 1000 in [
    WSDT.agree_test         ~count ~name:"ws_deque test";
    WSDT.agree_test_par          ~count ~name:"parallel ws_deque test";
  ])
