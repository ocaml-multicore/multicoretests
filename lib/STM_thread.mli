(** Module for building concurrent [STM] tests over {!Thread}s

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

module Make : functor (Spec : STM.Spec) ->
  sig
    exception ThreadNotFinished

    val arb_cmds_triple : int -> int -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary
    (** [arb_cmds_triple seq_len conc_len] generates a [cmd] triple with at most [seq_len]
        sequential commands and at most [conc_len] concurrent commands each.
        All [cmds] are generated with {!Spec.arb_cmd}.
        [arb_cmds_triple] catches and ignores generation-time exceptions arising
        from {!Spec.next_state}. *)

    val interp_sut_res : Spec.sut -> Spec.cmd list -> (Spec.cmd * STM.res) list
    (** [interp_sut_res sut cs] interprets the commands [cs] over the system [sut]
        and returns the list of corresponding {!Spec.cmd} and result pairs. *)

    val agree_prop_conc : Spec.cmd list * Spec.cmd list * Spec.cmd list -> bool
    (** Concurrent agreement property based on {!Thread} *)

    val agree_test_conc : count:int -> name:string -> QCheck.Test.t
    (** Concurrent agreement test based on {!Thread} which combines [repeat] and [~retries] *)

    val neg_agree_test_conc : count:int -> name:string -> QCheck.Test.t
    (** A negative agreement test (for convenience). Accepts two labeled parameters:
        [count] is the test count and [name] is the printed test name. *)

  end

module MakeExt : functor (Spec : STM.SpecExt) ->
  module type of Make (Spec)
