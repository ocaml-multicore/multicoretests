(** Module for building concurrent STM tests over [Thread]s *)

module Make : functor (Spec : STM_base.Spec) ->
  sig
    exception ThreadNotFinished

    val interp_sut_res : Spec.sut -> Spec.cmd list -> (Spec.cmd * STM_base.res) list
    (** [interp_sut_res sut cs] interprets the commands [cs] over the system [sut]
        and returns the list of corresponding [cmd] and result pairs. *)

    val agree_prop_conc : Spec.cmd list * Spec.cmd list * Spec.cmd list -> bool
    (** Concurrent agreement property based on [Thread] *)

    val agree_test_conc : count:int -> name:string -> QCheck.Test.t
    (** Concurrent agreement test based on [Thread] which combines [repeat] and [~retries] *)

    val neg_agree_test_conc : count:int -> name:string -> QCheck.Test.t
    (** A negative agreement test (for convenience). Accepts two labeled parameters:
        [count] is the test count and [name] is the printed test name. *)

  end
