open QCheck
open STM_base

module Lib : sig
  type 'a t
  val empty  : unit -> 'a t
  val elem   : 'a -> 'a t -> bool
  val add    : 'a -> 'a t -> unit
  val length : 'a t -> int
  val remove : 'a -> 'a t -> 'a option
end
= struct
  type 'a t = {
    mutable content : 'a list;
    mutable length  : int;
    mutex           : Mutex.t}

  let empty () = { content = []; length = 0; mutex = Mutex.create () }

  let elem_non_lock a t = List.mem a t.content

  let elem a t =
    Mutex.lock t.mutex;
    let b = elem_non_lock a t in
    Mutex.unlock t.mutex;
    b

  let add a t =
    Mutex.lock t.mutex;
    if not (elem_non_lock a t)
    then begin
      t.content <- a :: t.content;
      t.length  <- t.length + 1;
    end;
    Mutex.unlock t.mutex

  let length t =
    Mutex.lock t.mutex;
    let l = t.length in
    Mutex.unlock t.mutex;
    l

  let remove a t =
    Mutex.lock t.mutex;
    let r =
    if elem_non_lock a t
    then begin
      t.content <- List.filter (fun x -> x <> a) t.content;
      t.length  <- t.length - 1;
      Some a
    end
    else None
    in
    Mutex.unlock t.mutex;
    r
end


module Lib_spec : Spec = struct

  module State = Set.Make (struct type t = int let compare = Int.compare end)

  type sut        = int Lib.t
  let init_sut () = Lib.empty ()
  let cleanup _   = ()

  type cmd =
    | Elem of int
    | Add of int
    | Length
    | Remove of int [@@deriving show { with_path = false }]

  type state     = State.t
  let init_state = State.empty

  let arb_cmd state =
    let gen =
      if State.is_empty state
      then Gen.int
      else Gen.(oneof [oneofl (State.to_seq state |> List.of_seq); int])
    in
    QCheck.make ~print:show_cmd
      (QCheck.Gen.oneof
        [Gen.return Length;
         Gen.map (fun i -> Elem i) Gen.int;
         Gen.map (fun i -> Add i) Gen.int;
         Gen.map (fun i -> Remove i) gen;
        ])

  let next_state cmd state =
    match cmd with
    | Elem _   -> state
    | Add i    -> State.add i state
    | Length   -> state
    | Remove i -> State.remove i state

  let run cmd sut =
    match cmd with
    | Elem i   -> Res (bool, Lib.elem i sut)
    | Add i    -> Res (unit, Lib.add i sut)
    | Length   -> Res (int, Lib.length sut)
    | Remove i -> Res (option int, Lib.remove i sut)

  let precond _ _ = true

  let postcond cmd state res =
    match cmd, res with
    | Elem i,   Res ((Bool,_), b)            -> b = State.mem i state
    | Length,   Res ((Int,_), l)             -> l = State.cardinal state
    | Add _,    Res ((Unit,_),_)             -> true
    | Remove i, Res ((Option Int ,_),Some x) -> State.mem i state && i = x
    | Remove i, Res ((Option Int,_),None)    -> not (State.mem i state)
    | _                                      -> false
end

module Lib_domain = STM_domain.Make(Lib_spec)

let _ = QCheck_runner.run_tests ~verbose:true [Lib_domain.agree_test_par ~count:100 ~name:"STM parallel tests"]
