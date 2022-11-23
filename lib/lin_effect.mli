open Lin_base

(** functor to build an internal module representing effect-based tests *)
module Make_internal (Spec : Internal.CmdSpec) : sig
  module EffSpec : sig
    type cmd
  end
  val lin_prop_effect : (EffSpec.cmd list * EffSpec.cmd list * EffSpec.cmd list) -> bool
  val lin_test : count:int -> name:string -> QCheck.Test.t
  val neg_lin_test : count:int -> name:string -> QCheck.Test.t
end

val fork : (unit -> unit) -> unit
(** Helper function to fork a process in the underlying [Effect-based] scheduler
*)

val yield : unit -> unit
(** Helper function to yield control in the underlying [Effect-based] scheduler
*)

(** functor to build a module for [Effect]-based testing *)
module Make (Spec : ApiSpec) : sig
  val lin_test : count:int -> name:string -> QCheck.Test.t
  (** [lin_test ~count:c ~name:n] builds an [Effect]-based test with the name
      [n] that iterates [c] times. The test fails if one of the generated
      programs is not sequentially consistent. In that case it fails, and prints
      a reduced counter example.
  *)

  val neg_lin_test : count:int -> name:string -> QCheck.Test.t
  (** [neg_lin_test ~count:c ~name:n] builds a negative [Effect]-based test with
      the name [n] that iterates [c] times. The test fails if no counter example
      is found, and succeeds if a counter example is indeed found, and prints it
      afterwards.
  *)
end
