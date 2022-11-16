(** Module with combinators and definitions to specify an STM test *)

(** Extensible type to represent result values *)
type 'a ty = ..

(** A range of constructors to represent built-in types *)
type _ ty +=
  | Unit : unit ty
  | Bool : bool ty
  | Char : char ty
  | Int : int ty
  | Int32 : int32 ty
  | Int64 : int64 ty
  | Float : float ty
  | String : string ty
  | Bytes : bytes ty
  | Exn : exn ty
  | Option : 'a ty -> 'a option ty
  | Result : 'a ty * 'b ty -> ('a, 'b) result ty
  | List : 'a ty -> 'a list ty
  | Array : 'a ty -> 'a array ty
  | Seq : 'a ty -> 'a Seq.t ty

type 'a ty_show = 'a ty * ('a -> string)
(** Combinator type to represent an OCaml type along with an associated [to_string] function *)

val unit : unit ty_show
(** Combinator to represent the [unit] type *)

val bool : bool ty_show
(** Combinator to represent the [bool] type *)

val char : char ty_show
(** Combinator to represent the [char] type *)

val int : int ty_show
(** Combinator to represent the [int] type *)

val int32 : int32 ty_show
(** Combinator to represent the [int32] type *)

val int64 : int64 ty_show
(** Combinator to represent the [int64] type *)

val float : float ty_show
(** Combinator to represent the [float] type *)

val string : string ty_show
(** Combinator to represent the [string] type *)

val bytes : bytes ty_show
(** Combinator to represent the [bytes] type *)

val option : 'a ty_show -> 'a option ty_show
(** [option t] builds a [t option] type representation *)

val exn : exn ty_show
(** Combinator to represent the [exception] type *)

val result : 'a ty_show -> 'b ty_show -> ('a,'b) Result.t ty_show
(** [result a b] builds an [(a,b) Result.t] type representation *)

val list : 'a ty_show -> 'a list ty_show
(** [list t] builds a [t list] type representation *)

val array : 'a ty_show -> 'a array ty_show
(** [array t] builds a [t array] type representation *)

val seq : 'a ty_show -> 'a Seq.t ty_show
(** [seq t] builds a [t Seq.t] type representation *)

type res =
  Res : 'a ty_show * 'a -> res

val show_res : res -> string

(** The specification of a state machine. *)
module type Spec =
sig
  type cmd
  (** The type of commands *)

  type state
  (** The type of the model's state *)

  type sut
  (** The type of the system under test *)

  val arb_cmd : state -> cmd QCheck.arbitrary
  (** A command generator. Accepts a state parameter to enable state-dependent [cmd] generation. *)

  val init_state : state
  (** The model's initial state. *)

  val show_cmd : cmd -> string
  (** [show_cmd c] returns a string representing the command [c]. *)

  val next_state : cmd -> state -> state
  (** [next_state c s] expresses how interpreting the command [c] moves the
      model's internal state machine from the state [s] to the next state.
      Ideally a [next_state] function is pure. *)

  val init_sut : unit -> sut
  (** Initialize the system under test. *)

  val cleanup : sut -> unit
  (** Utility function to clean up the [sut] after each test instance,
      e.g., for closing sockets, files, or resetting global parameters*)

  val precond : cmd -> state -> bool
  (** [precond c s] expresses preconditions for command [c] in terms of the model state [s].
      A [precond] function should be pure.
      [precond] is useful, e.g., to prevent the shrinker from breaking invariants when minimizing
      counterexamples. *)

  val run : cmd -> sut -> res
  (** [run c i] should interpret the command [c] over the system under test (typically side-effecting). *)

  val postcond : cmd -> state -> res -> bool
  (** [postcond c s res] checks whether [res] arising from interpreting the
      command [c] over the system under test with [run] agrees with the
      model's result. A [postcond] function should be a pure.

      {b Note:} the state parameter [s] is the model's {!state} before executing the command [c] (the "old/pre" state).
      This is helpful to model, e.g., a [remove] [cmd] that returns the removed element. *)
end
