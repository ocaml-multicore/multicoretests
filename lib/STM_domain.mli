(** Module for building parallel STM tests over [Domain]s *)

module Make : functor (Spec : STM_base.Spec) ->
  sig
    val check_obs : (Spec.cmd * STM_base.res) list -> (Spec.cmd * STM_base.res) list -> (Spec.cmd * STM_base.res) list -> Spec.state -> bool
    (** [check_obs pref cs1 cs2 s] tests whether the observations from the sequential prefix [pref]
        and the parallel traces [cs1] [cs2] agree with the model started in state [s]. *)

    val arb_triple : int -> int -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary
    (** [arb_triple seq_len par_len arb0 arb1 arb2] generates a [cmd] triple with at most [seq_len]
        sequential commands and at most [par_len] parallel commands each.
        The three [cmd] components are generated with [arb0], [arb1], and [arb2], respectively.
        Each of these take the model state as a parameter. *)

    val shrink_triple : (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.Shrink.t
    (** [shrink_triple arb0 arb1 arb2] is a [Shrinker.t] for programs (triple of list of [cmd]s) that is specialized for each part of the program. *)

    val interp_sut_res : Spec.sut -> Spec.cmd list -> (Spec.cmd * STM_base.res) list
    (** [interp_sut_res sut cs] interprets the commands [cs] over the system [sut]
        and returns the list of corresponding [cmd] and result pairs. *)

    val agree_prop_par : Spec.cmd list * Spec.cmd list * Spec.cmd list -> bool
    (** Parallel agreement property based on [Domain] *)

    val agree_test_par : count:int -> name:string -> QCheck.Test.t
    (** Parallel agreement test based on [Domain] which combines [repeat] and [~retries] *)

    val neg_agree_test_par : count:int -> name:string -> QCheck.Test.t
    (** A negative agreement test (for convenience). Accepts two labeled parameters:
        [count] is the test count and [name] is the printed test name. *)

 end
