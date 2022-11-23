open Lin_base

(** functor to build an internal module representing parallel tests *)
module Make_internal (Spec : Lin_internal.CmdSpec) : sig
  val arb_cmds_par : int -> int -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary
  val lin_prop_domain : (Spec.cmd list * Spec.cmd list * Spec.cmd list) -> bool
  val lin_test : count:int -> name:string -> QCheck.Test.t
  val neg_lin_test : count:int -> name:string -> QCheck.Test.t
end

(** functor to build a module for parallel testing *)
module Make (Spec : ApiSpec) : sig
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
end
