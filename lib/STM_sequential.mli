(** Module for building sequential STM tests *)

module Make : functor (Spec : STM_base.Spec) ->
  sig
    val cmds_ok : Spec.state -> Spec.cmd list -> bool
    (** A precondition checker (stops early, thanks to short-circuit Boolean evaluation).
        Accepts the initial state and the command sequence as parameters.  *)

    val gen_cmds_size : (Spec.state -> Spec.cmd QCheck.arbitrary) -> Spec.state -> int QCheck.Gen.t -> Spec.cmd list QCheck.Gen.t
    (** [gen_cmds_size arb state gen_int] generates a program of size generated
        by [gen_int] using [arb] to generate [cmd]s according to the current
        state. [state] is the starting state. *)

    val agree_prop : Spec.cmd list -> bool
    (** The agreement property: the command sequence [cs] yields the same observations
        when interpreted from the model's initial state and the [sut]'s initial state.
        Cleans up after itself by calling [Spec.cleanup] *)

    val agree_test : count:int -> name:string -> QCheck.Test.t
    (** An actual agreement test (for convenience). Accepts two labeled parameters:
        [count] is the test count and [name] is the printed test name. *)

    val neg_agree_test : count:int -> name:string -> QCheck.Test.t
    (** A negative agreement test (for convenience). Accepts two labeled parameters:
        [count] is the test count and [name] is the printed test name. *)
  end
