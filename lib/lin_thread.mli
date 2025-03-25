(** Module for building concurrent [Lin] tests over {!Thread}s

    Context switches in {!Thread}s may happen
    - at allocations and
    - at safepoints {:https://github.com/ocaml/ocaml/pull/10039}.

    This module relies on [Gc.Memprof] support to trigger more frequent context
    switching between threads at allocation sites. This works well in OCaml
    4.11.0-4.14.x and 5.3.0 onwards where [Gc.Memprof] is available.

    In OCaml 5.0-5.2 without [Gc.Memprof] support the context switching at
    allocation sites will be inferior. As a consequence the module may fail to
    trigger concurrency issues.

    Context switches at safepoints will trigger much less frequently. This
    means the module may fail to trigger concurrency issues in connection with
    these. Consider yourself warned.
*)

open Lin

(** Functor to build an internal module representing concurrent tests *)
module Make_internal (Spec : Internal.CmdSpec [@alert "-internal"]) : sig
  val arb_cmds_triple : int -> int -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary
  val lin_prop : (Spec.cmd list * Spec.cmd list * Spec.cmd list) -> bool
  val lin_test : count:int -> name:string -> QCheck.Test.t
  val neg_lin_test : count:int -> name:string -> QCheck.Test.t
end
  [@@alert internal "This module is exposed for internal uses only, its API may change at any time"]

(** Functor to build a module for concurrent testing *)
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
end
