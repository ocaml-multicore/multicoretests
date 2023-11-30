open Lin

(** functor to build an internal module representing concurrent tests *)
module Make_internal (Spec : Internal.CmdSpec [@alert "-internal"]) : sig
  val arb_cmds_triple : int -> int -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary
  val lin_prop : (Spec.cmd list * Spec.cmd list * Spec.cmd list) -> bool
  val lin_test : count:int -> name:string -> QCheck.Test.t
  val neg_lin_test : count:int -> name:string -> QCheck.Test.t
  val lin_stats : count:int -> int
end
  [@@alert internal "This module is exposed for internal uses only, its API may change at any time"]

(** functor to build a module for concurrent testing *)
module Make (Spec : Spec) : sig
  val lin_test : count:int -> name:string -> QCheck.Test.t
  (** [lin_test ~count:c ~name:n] builds a concurrent test with the name [n]
      that iterates [c] times. The test fails if one of the generated programs
      is not sequentially consistent. In that case it fails, and prints a
      reduced counter example.
  *)

  val neg_lin_test : count:int -> name:string -> QCheck.Test.t
  (** [neg_lin_test ~count:c ~name:n] builds a negative concurrent test with
      the name [n] that iterates [c] times. The test fails if no counter example
      is found, and succeeds if a counter example is indeed found, and prints it
      afterwards.
  *)

  val lin_stats : count:int -> int
  (** Repeatedly run a concurrent test based on {!Stdlib.Thread} and
      return how many times sequential consistency failed.
      Accepts a labeled parameter:
      [count] is the number of test iterations. *)
end
[@@alert experimental "This module is experimental: It may fail to trigger concurrency issues that are present."]
