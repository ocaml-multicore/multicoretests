(** Module for building sequential STM tests *)

module Make : functor (Spec : STM.Spec) ->
  sig
    type cmd_res = Pack_cmd_res : 'a Spec.cmd * 'a STM.res -> cmd_res

    val cmds_ok : Spec.state -> Spec.Cmd.any list -> bool
    (** A precondition checker (stops early, thanks to short-circuit Boolean evaluation).
        Accepts the initial state and the command sequence as parameters.
        [cmds_ok] catches and ignores exceptions arising from {!next_state}.  *)

    val arb_cmds : Spec.state -> Spec.Cmd.any list QCheck.arbitrary
    (** A generator of {!Spec.cmd} sequences. Accepts the initial state as a parameter.
        [arb_cmds] catches and ignores generation-time exceptions arising from
        {!Spec.next_state}. *)

    val agree_prop : Spec.Cmd.any list -> bool
    (** The agreement property: the command sequence [cs] yields the same observations
        when interpreted from the model's initial state and the [sut]'s initial state.
        Cleans up after itself by calling {!Spec.cleanup}. *)

    val agree_test : count:int -> name:string -> QCheck.Test.t
    (** An actual agreement test (for convenience). Accepts two labeled parameters:
        [count] is the test count and [name] is the printed test name. *)

    val neg_agree_test : count:int -> name:string -> QCheck.Test.t
    (** A negative agreement test (for convenience). Accepts two labeled parameters:
        [count] is the test count and [name] is the printed test name. *)
  end
