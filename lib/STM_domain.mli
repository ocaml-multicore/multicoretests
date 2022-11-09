open STM_base

module Make : functor (Spec : STM_spec.Spec) ->
  sig
    val arb_triple : int -> int -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary
    (** [arb_cmds_par seq_len par_len arb0 arb1 arb2] generates a [cmd] triple with at most [seq_len]
        sequential commands and at most [par_len] parallel commands each.
        The three [cmd] components are generated with [arb0], [arb1], and [arb2], respectively.
        Each of these take the model state as a parameter. *)

    val interp_sut_res : Spec.sut -> Spec.cmd list -> (Spec.cmd * res) list
    (** [interp_sut_res sut cs] interprets the commands [cs] over the system [sut]
        and returns the list of corresponding [cmd] and result pairs. *)

    val agree_prop_par : Spec.cmd list * Spec.cmd list * Spec.cmd list -> bool
    (** Parallel agreement property based on [Domain] *)

    val agree_test_par : count:int -> name:string -> QCheck.Test.t
    (** Parallel agreement test based on [Domain] which combines [repeat] and [~retries] *)

    val neg_agree_test_par : count:int -> name:string -> QCheck.Test.t
    (** An negative agreement test (for convenience). Accepts two labeled parameters:
        [count] is the test count and [name] is the printed test name. *)

 end
