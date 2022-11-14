open QCheck
open STM_spec
(** Internal helper module to build STM tests. *)


(** Derives a test framework from a state machine specification. *)
module Make : functor (Spec : Spec) ->
  sig
  (** {3 The resulting test framework derived from a state machine specification} *)

    val cmds_ok : Spec.state -> Spec.cmd list -> bool
    (** A precondition checker (stops early, thanks to short-circuit Boolean evaluation).
        Accepts the initial state and the command sequence as parameters.  *)

    val arb_cmds : Spec.state -> Spec.cmd list arbitrary
    (** A generator of command sequences. Accepts the initial state as parameter. *)

    val consistency_test : count:int -> name:string -> QCheck.Test.t
    (** A consistency test that generates a number of [cmd] sequences and
        checks that all contained [cmd]s satisfy the precondition [precond].
        Accepts two labeled parameters:
        [count] is the test count and [name] is the printed test name. *)

    val interp_agree : Spec.state -> Spec.sut -> Spec.cmd list -> bool
    (** Checks agreement between the model and the system under test
        (stops early, thanks to short-circuit Boolean evaluation). *)

    val check_disagree : Spec.state -> Spec.sut -> Spec.cmd list -> (Spec.cmd * res) list option
    (** [check_disagree state sut pg] checks that none of the commands present
        in [pg] violated the declared postconditions when [pg] is run in [state].
        Return [None] if none of the commands violate its postcondition, and
        [Some] list corresponding to the prefix of [pg] ending with the [cmd]
        violating its postcondition. *)

    val check_obs : (Spec.cmd * res) list -> (Spec.cmd * res) list -> (Spec.cmd * res) list -> Spec.state -> bool
    (** [check_obs pref cs1 cs2 s] tests whether the observations from the sequential prefix [pref]
        and the parallel traces [cs1] [cs2] agree with the model started in state [s]. *)

    val gen_cmds_size : (Spec.state -> Spec.cmd arbitrary) -> Spec.state -> int Gen.t -> Spec.cmd list Gen.t
    (** [gen_cmds_size arb state gen_int] generates a program of size generated
        by [gen_int] using [arb] to generate [cmd]s according to the current
        state. [state] is the starting state. *)

    val arb_cmds_par : int -> int -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) arbitrary
    (** [arb_cmds_par seq_len par_len] generates a [cmd] triple with at most [seq_len]
        sequential commands and at most [par_len] parallel commands each. *)

    val all_interleavings_ok : Spec.cmd list -> Spec.cmd list -> Spec.cmd list -> Spec.state -> bool
    (** [all_interleavings_ok seq spawn0 spawn1 state] checks that
        preconditions of all the [cmd]s of [seq], [spawn0], and [spawn1] are satisfied in all the
        possible interleavings and starting with [state] *)

    val shrink_triple : (Spec.state -> Spec.cmd arbitrary) -> (Spec.state -> Spec.cmd arbitrary) -> (Spec.state -> Spec.cmd arbitrary) -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) Shrink.t
    (** [shrink_triple arb0 arb1 arb2] is a [Shrinker.t] for programs (triple of list of [cmd]s) that is specialized for each part of the program. *)

    val arb_triple : int -> int -> (Spec.state -> Spec.cmd arbitrary) -> (Spec.state -> Spec.cmd arbitrary) -> (Spec.state -> Spec.cmd arbitrary) -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) arbitrary
    (** [arb_triple seq_len par_len arb0 arb1 arb2] generates a [cmd] triple with at most [seq_len]
        sequential commands and at most [par_len] parallel commands each.
        The three [cmd] components are generated with [arb0], [arb1], and [arb2], respectively.
        Each of these take the model state as a parameter. *)

  end
