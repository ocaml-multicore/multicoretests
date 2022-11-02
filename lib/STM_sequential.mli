open STM_base

module Make : functor (Spec : STM_spec.Spec) ->
  sig
    val cmds_ok : Spec.state -> Spec.cmd list -> bool
    (** A precondition checker (stops early, thanks to short-circuit Boolean evaluation).
        Accepts the initial state and the command sequence as parameters.  *)

    val check_obs : (Spec.cmd * res) list -> (Spec.cmd * res) list -> (Spec.cmd * res) list -> Spec.state -> bool
    (** [check_obs pref cs1 cs2 s] tests whether the observations from the sequential prefix [pref]
        and the parallel traces [cs1] [cs2] agree with the model started in state [s]. *)

    val gen_cmds_size : (Spec.state -> Spec.cmd QCheck.arbitrary) -> Spec.state -> int QCheck.Gen.t -> Spec.cmd list QCheck.Gen.t

    val shrink_triple : (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.Shrink.t

    val interp_sut_res : Spec.sut -> Spec.cmd list -> (Spec.cmd * res) list
    (** [interp_sut_res sut cs] interprets the commands [cs] over the system [sut]
        and returns the list of corresponding [cmd] and result pairs. *)

    val agree_prop : Spec.cmd list -> bool
    (** The agreement property: the command sequence [cs] yields the same observations
        when interpreted from the model's initial state and the [sut]'s initial state.
        Cleans up after itself by calling [Spec.cleanup] *)

    val agree_test : count:int -> name:string -> QCheck2.Test.t
    (** An actual agreement test (for convenience). Accepts two labeled parameters:
        [count] is the test count and [name] is the printed test name. *)

  end
