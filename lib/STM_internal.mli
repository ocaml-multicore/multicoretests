open QCheck
open STM_spec
(** A revised state machine framework with parallel testing.
    This version does not come with built-in GC commands. *)


(** Derives a test framework from a state machine specification. *)
module Make : functor (Spec : Spec) ->
  sig
  (** {3 The resulting test framework derived from a state machine specification} *)

    val cmds_ok : Spec.state -> Spec.cmd list -> bool
    (** A precondition checker (stops early, thanks to short-circuit Boolean evaluation).
        Accepts the initial state and the command sequence as parameters.  *)

    val arb_cmds : Spec.state -> Spec.cmd list arbitrary
    (** A generator of command sequences. Accepts the initial state as parameter. *)

    val interp_agree : Spec.state -> Spec.sut -> Spec.cmd list -> bool
    (** Checks agreement between the model and the system under test
        (stops early, thanks to short-circuit Boolean evaluation). *)

    val check_disagree : Spec.state -> Spec.sut -> Spec.cmd list -> (Spec.cmd * res) list option

    val check_obs : (Spec.cmd * res) list -> (Spec.cmd * res) list -> (Spec.cmd * res) list -> Spec.state -> bool
    (** [check_obs pref cs1 cs2 s] tests whether the observations from the sequential prefix [pref]
        and the parallel traces [cs1] [cs2] agree with the model started in state [s]. *)

    val gen_cmds_size : (Spec.state -> Spec.cmd arbitrary) -> Spec.state -> int Gen.t -> Spec.cmd list Gen.t

    val arb_cmds_par : int -> int -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) arbitrary
    (** [arb_cmds_par seq_len par_len] generates a [cmd] triple with at most [seq_len]
        sequential commands and at most [par_len] parallel commands each. *)

    val all_interleavings_ok : Spec.cmd list -> Spec.cmd list -> Spec.cmd list -> Spec.state -> bool

    val shrink_triple : (Spec.state -> Spec.cmd arbitrary) -> (Spec.state -> Spec.cmd arbitrary) -> (Spec.state -> Spec.cmd arbitrary) -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) Shrink.t

    val arb_triple : int -> int -> (Spec.state -> Spec.cmd arbitrary) -> (Spec.state -> Spec.cmd arbitrary) -> (Spec.state -> Spec.cmd arbitrary) -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) arbitrary
    (** [arb_triple seq_len par_len arb0 arb1 arb2] generates a [cmd] triple with at most [seq_len]
        sequential commands and at most [par_len] parallel commands each.
        The three [cmd] components are generated with [arb0], [arb1], and [arb2], respectively.
        Each of these take the model state as a parameter. *)


  end
