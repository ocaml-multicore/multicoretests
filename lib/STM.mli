(** A state machine framework supporting both sequential and parallel model-based testing. *)

include module type of Util
(** Sub-module of various utility functions *)

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

  val next_state : cmd -> state -> state
  (** Move the internal state machine to the next state. *)

  val init_sut : unit -> sut
  (** Initialize the system under test. *)

  val cleanup : sut -> unit
  (** Utility function to clean up the [sut] after each test instance,
      e.g., for closing sockets, files, or resetting global parameters*)

  val precond : cmd -> state -> bool
  (** [precond c s] expresses preconditions for command [c].
      This is useful, e.g., to prevent the shrinker from breaking invariants when minimizing
      counterexamples. *)

  val show_cmd : cmd -> string
  (** [show_cmd c] returns a string representing the command [c]. *)

  val run : cmd -> sut -> res
  (** [run c i] should interpret the command [c] over the system under test (typically side-effecting). *)

  val postcond : cmd -> state -> res -> bool
  (** [postcond c s res] checks whether [res] arising from interpreting the
      command [c] over the system under test with [run] agrees with the
      model's result.
      Note: [s] is in this case the model's state prior to command execution. *)
end

module AddGC : functor (Spec: Spec) -> Spec
module STM_Seq : sig
  module Make : functor (Spec: Spec) -> sig
                  val cmds_ok: Spec.state -> Spec.cmd list -> bool
                  (** A precondition checker (stops early, thanks to short-circuit Boolean evaluation).
                      Accepts the initial state and the command sequence as parameters.  *)

                  val interp_sut_res: Spec.sut -> Spec.cmd list -> (Spec.cmd * res) list
                  (** [interp_sut_res sut cs] interprets the commands [cs] over the system [sut]
                      and returns the list of corresponding [cmd] and result pairs. *)

                  val check_obs: (Spec.cmd * res) list ->
                                 (Spec.cmd * res) list ->
                                 (Spec.cmd * res) list ->
                                 Spec.state -> bool
                  (** [check_obs pref cs1 cs2 s] tests whether the observations from the sequential prefix [pref]
                      and the parallel traces [cs1] [cs2] agree with the model started in state [s]. *)

                  val gen_cmds_size: (Spec.state -> Spec.cmd QCheck.arbitrary) -> Spec.state -> int QCheck.Gen.t -> Spec.cmd list QCheck.Gen.t
                  val agree_prop: Spec.cmd list -> bool
                  (** The agreement property: the command sequence [cs] yields the same observations
                      when interpreted from the model's initial state and the [sut]'s initial state.
                      Cleans up after itself by calling [Spec.cleanup] *)

                  val agree_test: count:int -> name:string -> QCheck.Test.t
                  (** An actual agreement test (for convenience). Accepts two labeled parameters:
                      [count] is the test count and [name] is the printed test name. *)

                  val shrink_triple : (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.Shrink.t
                end
end

module STM_Domain : sig
  module Make : functor (Spec: Spec) -> sig
                  val arb_triple: int -> int -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary

                  val agree_prop_par: (Spec.cmd list * Spec.cmd list * Spec.cmd list) -> bool
                  (** Parallel agreement property based on [Domain] *)

                  val agree_test_par: count:int -> name:string -> QCheck.Test.t
                  (** Parallel agreement test based on [Domain] which combines [repeat] and [~retries] *)

                  val neg_agree_test_par: count:int -> name:string -> QCheck.Test.t
                  (** Negative concurrent agreement test based on [Domain] which combines [repeat] and [~retries] *)
                end
end

module STM_Thread : sig
  module Make : functor (Spec: Spec) -> sig
                  val agree_prop_conc: (Spec.cmd list * Spec.cmd list * Spec.cmd list) -> bool
                  (** Concurrent agreement property based on [Thread] *)

                  val agree_test_conc: count:int -> name:string -> QCheck.Test.t
                  (** Concurrent agreement test based on [Thread] which combines [repeat] and [~retries] *)

                  val neg_agree_test_conc: count:int -> name:string -> QCheck.Test.t
                  (** Negative concurrent agreement test based on [Thread] which combines [repeat] and [~retries] *)
                end
end
