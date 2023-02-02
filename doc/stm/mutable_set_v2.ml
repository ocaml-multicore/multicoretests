module type S = sig
  type elt
  type t
  val empty    : unit -> t
  val mem      : elt -> t -> bool
  val add      : elt -> t -> unit
  val cardinal : t -> int
end

module Lib : sig
  module Make : functor (Ord : Set.OrderedType) -> S with type elt = Ord.t
end
= struct
  module Make (Ord : Set.OrderedType) =
    struct
    module S = Set.Make (Ord)

    type elt = Ord.t

    type t = {
      mutable content  : S.t;
      mutable cardinal : int }

    let empty () = { content = S.empty; cardinal = 0 }

    let mem a t = S.mem a t.content

    let add a t =
      if not (mem a t)
      then begin
        t.content <- S.add a t.content;
        t.cardinal <- t.cardinal + 1
      end

    let cardinal t = t.cardinal
  end
end

open QCheck
open STM

module Lib_spec : Spec = struct

  module S = Lib.Make (Int)

  type sut = S.t
  let init_sut () = S.empty ()
  let cleanup _ = ()

  type cmd =
    | Mem of int
    | Add of int
    | Cardinal [@@deriving show { with_path = false }]

  let run cmd sut =
    match cmd with
    | Mem i    -> Res (bool, S.mem i sut)
    | Add i    -> Res (unit, S.add i sut)
    | Cardinal -> Res (int, S.cardinal sut)

  type state = int list
  let init_state = []

  let next_state cmd state =
    match cmd with
    | Mem _    -> state
    | Add i    -> if List.mem i state then state else i :: state
    | Cardinal -> state

  let precond _cmd _state = true

  let postcond cmd state res =
    match cmd, res with
    | Mem i,  Res ((Bool,_), b)  -> b = List.mem i state
    | Cardinal, Res ((Int,_), l) -> l = List.length state
    | Add _,  Res ((Unit,_),_)   -> true
    | _                          -> false

  let arb_cmd _state =
    QCheck.make ~print:show_cmd
      (QCheck.Gen.oneof
        [Gen.return Cardinal;
         Gen.map (fun i -> Mem i) Gen.int;
         Gen.map (fun i -> Add i) Gen.int;
        ])
end

module Lib_domain = STM_domain.Make(Lib_spec)

let _ =
  QCheck_base_runner.run_tests ~verbose:true
    [Lib_domain.agree_test_par ~count:100 ~name:"STM parallel tests"]
