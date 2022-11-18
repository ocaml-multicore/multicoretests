open QCheck
open STM_base

module Lib : sig
  type 'a t
  val empty  : unit -> 'a t
  val elem   : 'a -> 'a t -> bool
  val add    : 'a -> 'a t -> unit
  val length : 'a t -> int
end
= struct
  type 'a t = {
    mutable content : 'a list;
    mutable length  : int }

  let empty () = { content = []; length = 0 }

  let elem a t = List.mem a t.content

  let add a t =
    if not (elem a t)
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
    | Elem of int
    | Add of int
    | Length [@@deriving show { with_path = false }]

  type state     = State.t
  let init_state = State.empty

  let arb_cmd _state =
    QCheck.make ~print:show_cmd
      (QCheck.Gen.oneof
        [Gen.return Length;
         Gen.map (fun i -> Elem i) Gen.int;
         Gen.map (fun i -> Add i) Gen.int;
        ])

  let next_state cmd state =
    match cmd with
    | Elem _   -> state
    | Add i    -> State.add i state
    | Length   -> state

  let run cmd sut =
    match cmd with
    | Elem i   -> Res (bool, Lib.elem i sut)
    | Add i    -> Res (unit, Lib.add i sut)
    | Length   -> Res (int, Lib.length sut)

  let precond _ _ = true

  let postcond cmd state res =
    match cmd, res with
    | Elem i,   Res ((Bool,_), b) -> b = State.mem i state
    | Length,   Res ((Int,_), l)  -> l = State.cardinal state
    | Add _,    Res ((Unit,_),_)  -> true
    | _                            -> false
end

module Lib_domain = STM_domain.Make(Lib_spec)

let _ = QCheck_runner.run_tests ~verbose:true [Lib_domain.agree_test_par ~count:100 ~name:"STM parallel tests"]
