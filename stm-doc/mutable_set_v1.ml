open QCheck
open STM_base

module Lib : sig
  type 'a t
  val empty  : unit -> 'a t
  val mem    : 'a -> 'a t -> bool
  val add    : 'a -> 'a t -> unit
  val length : 'a t -> int
end
= struct
  type 'a t = {
    mutable content : 'a list;
    mutable length  : int }

  let empty () = { content = []; length = 0 }

  let mem a t = List.mem a t.content

  let add a t =
    if not (mem a t)
    then begin
      t.content <- a :: t.content;
      t.length  <- t.length + 1;
    end

  let length t = t.length
end


module Lib_spec : Spec = struct

  module State = Set.Make (struct type t = int let compare = Int.compare end)

  type sut        = int Lib.t
  let init_sut () = Lib.empty ()
  let cleanup _   = ()

  type cmd =
    | Mem of int
    | Add of int
    | Length [@@deriving show { with_path = false }]

  type state     = State.t
  let init_state = State.empty

  let arb_cmd _state =
    QCheck.make ~print:show_cmd
      (QCheck.Gen.oneof
        [Gen.return Length;
         Gen.map (fun i -> Mem i) Gen.int;
         Gen.map (fun i -> Add i) Gen.int;
        ])

  let next_state cmd state =
    match cmd with
    | Mem _  -> state
    | Add i  -> State.add i state
    | Length -> state

  let run cmd sut =
    match cmd with
    | Mem i  -> Res (bool, Lib.mem i sut)
    | Add i  -> Res (unit, Lib.add i sut)
    | Length -> Res (int, Lib.length sut)

  let precond _ _ = true

  let postcond cmd state res =
    match cmd, res with
    | Mem i,  Res ((Bool,_), b)-> b = State.mem i state
    | Length, Res ((Int,_), l) -> l = State.cardinal state
    | Add _,  Res ((Unit,_),_) -> true
    | _                        -> false
end

module Lib_sequential = STM_sequential.Make(Lib_spec)

let _ = QCheck_runner.run_tests ~verbose:true [Lib_sequential.agree_test ~count:100 ~name:"STM sequential tests"]
