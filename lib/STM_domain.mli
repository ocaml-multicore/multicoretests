(** Module for building parallel STM tests over {!Stdlib.Domain}s *)

module Make : functor (Spec : STM.Spec) ->
  sig
    val check_obs : (Spec.cmd * STM.res) list -> (Spec.cmd * STM.res) list -> (Spec.cmd * STM.res) list -> Spec.state -> bool
    (** [check_obs pref cs1 cs2 s] tests whether the observations from the sequential prefix [pref]
        and the parallel traces [cs1] [cs2] agree with the model started in state [s]. *)

    val all_interleavings_ok : (Spec.cmd list * Spec.cmd list * Spec.cmd list) -> bool
    (** [all_interleavings_ok (seq,spawn0,spawn1)] checks that
        preconditions of all the {!cmd}s of [seq], [spawn0], and [spawn1] are satisfied in all the
        possible interleavings and starting with {!Spec.init_state}.
        [all_interleavings_ok] catches and ignores exceptions arising from
        {!next_state}. *)

    val arb_cmds_triple : int -> int -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary
    (** [arb_cmds_triple seq_len par_len] generates a [cmd] triple with at most [seq_len]
        sequential commands and at most [par_len] parallel commands each.
        All [cmds] are generated with {!Spec.arb_cmd}.
        [arb_cmds_triple] catches and ignores generation-time exceptions arising
        from {!Spec.next_state}. *)

    val arb_triple : int -> int -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary
    (** [arb_triple seq_len par_len arb0 arb1 arb2] generates a [cmd] triple with at most [seq_len]
        sequential commands and at most [par_len] parallel commands each.
        The three {!Spec.cmd} components are generated with [arb0], [arb1], and [arb2], respectively.
        Each of these take the model state as a parameter.
        [arb_triple] catches and ignores generation-time exceptions arising
        from {!Spec.next_state}. *)

    val arb_triple_asym : int -> int -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary
    (** [arb_triple_asym seq_len par_len arb0 arb1 arb2] creates a triple [cmd]
        generator like {!arb_triple}. It differs in that the resulting printer
        is asymmetric, printing [arb1]'s result below [arb0]'s result and
        printing [arb2]'s result to the right of [arb1]'s result.
        [arb_triple_asym] catches and ignores generation-time exceptions arising
        from {!Spec.next_state}. *)

    val interp_sut_res : Spec.sut -> Spec.cmd list -> (Spec.cmd * STM.res) list
    (** [interp_sut_res sut cs] interprets the commands [cs] over the system {!Spec.sut}
        and returns the list of corresponding {!Spec.cmd} and result pairs. *)

    val stress_prop_par : Spec.cmd list * Spec.cmd list * Spec.cmd list -> bool
    (** Parallel stress testing property based on {!Stdlib.Domain}.
        [stress_prop_par (seq_pref, tl1, tl2)] first interprets [seq_pref]
        and then spawns two parallel, symmetric domains interpreting [tl1] and
        [tl2] simultaneously. In contrast to {!agree_prop_par}, [stress_prop_par]
        does not perform an interleaving search.

        @return [true] if no unexpected exceptions or crashes are encountered. *)

    val agree_prop_par : Spec.cmd list * Spec.cmd list * Spec.cmd list -> bool
    (** Parallel agreement property based on {!Stdlib.Domain}.
        [agree_prop_par (seq_pref, tl1, tl2)] first interprets [seq_pref]
        and then spawns two parallel, symmetric domains interpreting [tl1] and
        [tl2] simultaneously.

        @return [true] if there exists a sequential interleaving of the results
        which agrees with a model interpretation. *)

    val agree_prop_par_asym : Spec.cmd list * Spec.cmd list * Spec.cmd list -> bool
    (** Asymmetric parallel agreement property based on {!Stdlib.Domain}.
        [agree_prop_par_asym (seq_pref, tl1, tl2)] first interprets [seq_pref],
        and then interprets [tl1] while a spawned domain interprets [tl2]
        in parallel with the parent domain.

        @return [true] if there exists a sequential interleaving of the results
        which agrees with a model interpretation. *)

    val agree_test_par : count:int -> name:string -> QCheck.Test.t
    (** Parallel agreement test based on {!Stdlib.Domain} which combines [repeat] and [~retries].
        Accepts two labeled parameters:
        [count] is the number of test iterations and [name] is the printed test name. *)

    val neg_agree_test_par : count:int -> name:string -> QCheck.Test.t
    (** A negative parallel agreement test (for convenience). Accepts two labeled parameters:
        [count] is the number of test iterations and [name] is the printed test name. *)

    val agree_test_par_asym : count:int -> name:string -> QCheck.Test.t
    (** Asymmetric parallel agreement test based on {!Stdlib.Domain} and {!agree_prop_par_asym}
        which combines [repeat] and [~retries]. Accepts two labeled parameters:
        [count] is the number of test iterations and [name] is the printed test name. *)

    val neg_agree_test_par_asym : count:int -> name:string -> QCheck.Test.t
    (** A negative asymmetric parallel agreement test (for convenience).
        Accepts two labeled parameters:
        [count] is the number of test iterations and [name] is the printed test name. *)

    val stress_test_par : count:int -> name:string -> QCheck.Test.t
    (** Parallel stress test based on {!Stdlib.Domain} which combines [repeat] and [~retries].
        Accepts two labeled parameters:
        [count] is the number of test iterations and [name] is the printed test name.
        The test fails if an unexpected exception is raised underway. It is
        intended as a stress test to run operations at a high frequency and
        detect unexpected exceptions or crashes. It does not perform an
        interleaving search like {!agree_test_par} and {!neg_agree_test_par}. *)

 end
