open Lin

(** functor to build an internal module representing parallel tests *)
module Make_internal (Spec : Internal.CmdSpec [@alert "-internal"]) : sig
  val arb_cmds_triple : int -> int -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary
  val lin_prop : (Spec.cmd list * Spec.cmd list * Spec.cmd list) -> bool
  val stress_prop : (Spec.cmd list * Spec.cmd list * Spec.cmd list) -> bool
  val lin_test : count:int -> name:string -> QCheck.Test.t
  val neg_lin_test : count:int -> name:string -> QCheck.Test.t
  val stress_test : count:int -> name:string -> QCheck.Test.t
end
  [@@alert internal "This module is exposed for internal uses only, its API may change at any time"]

(** functor to build a module for parallel testing *)
module Make (Spec : Spec) : sig
  val lin_test : count:int -> name:string -> QCheck.Test.t
  (** [lin_test ~count:c ~name:n] builds a parallel test with the name [n] that
      iterates [c] times. The test fails if one of the generated programs is not
      sequentially consistent. In that case it fails, and prints a reduced
      counter example.
  *)

  val neg_lin_test : count:int -> name:string -> QCheck.Test.t
  (** [neg_lin_test ~count:c ~name:n] builds a negative parallel test with the
      name [n] that iterates [c] times. The test fails if no counter example is
      found, and succeeds if a counter example is indeed found, and prints it
      afterwards.
  *)

  val stress_test : count:int -> name:string -> QCheck.Test.t
  (** [stress_test ~count:c ~name:n] builds a parallel test with the name
      [n] that iterates [c] times. The test fails if an unexpected exception is
      raised underway. It is intended as a stress test to run operations at a
      high frequency and detect unexpected exceptions or crashes. It does not
      perform an interleaving search like {!lin_test} and {!neg_lin_test}. *)
end
