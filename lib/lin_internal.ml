open QCheck
include Util

module type CmdSpec = sig
  type t
  (** The type of the system under test *)

  type cmd
  (** The type of commands *)

  val show_cmd : cmd -> string
  (** [show_cmd c] returns a string representing the command [c]. *)

  val gen_cmd : cmd Gen.t
  (** A command generator. *)

  val shrink_cmd : cmd Shrink.t
  (** A command shrinker.
      To a first approximation you can use [Shrink.nil]. *)

  type res
  (** The command result type *)

  val show_res : res -> string
  (** [show_res r] returns a string representing the result [r]. *)

  val equal_res : res -> res -> bool

  val init : unit -> t
  (** Initialize the system under test. *)

  val cleanup : t -> unit
  (** Utility function to clean up [t] after each test instance,
      e.g., for closing sockets, files, or resetting global parameters *)

  val run : cmd -> t -> res
  (** [run c t] should interpret the command [c] over the system under test [t] (typically side-effecting). *)
end

(** A functor to create test setups, for all backends (Domain, Thread and Effect).
    We use it below, but it can also be used independently *)
module Make(Spec : CmdSpec)
  = struct

  (* plain interpreter of a cmd list *)
  let interp_plain sut cs = List.map (fun c -> (c, Spec.run c sut)) cs

  let rec gen_cmds fuel =
    Gen.(if fuel = 0
         then return []
         else
  	  Spec.gen_cmd >>= fun c ->
	   gen_cmds (fuel-1) >>= fun cs ->
             return (c::cs))
  (** A fueled command list generator. *)

  let gen_cmds_size size_gen = Gen.sized_size size_gen gen_cmds

  let shrink_triple (seq,p1,p2) =
    let open Iter in
    (* Shrinking heuristic:
       First reduce the cmd list sizes as much as possible, since the interleaving
       is most costly over long cmd lists. *)
    (map (fun seq' -> (seq',p1,p2)) (Shrink.list_spine seq))
    <+>
    (match p1 with [] -> Iter.empty | c1::c1s -> Iter.return (seq@[c1],c1s,p2))
    <+>
    (match p2 with [] -> Iter.empty | c2::c2s -> Iter.return (seq@[c2],p1,c2s))
    <+>
    (map (fun p1' -> (seq,p1',p2)) (Shrink.list_spine p1))
    <+>
    (map (fun p2' -> (seq,p1,p2')) (Shrink.list_spine p2))
    <+>
    (* Secondly reduce the cmd data of individual list elements *)
    (map (fun seq' -> (seq',p1,p2)) (Shrink.list_elems Spec.shrink_cmd seq))
    <+>
    (map (fun p1' -> (seq,p1',p2)) (Shrink.list_elems Spec.shrink_cmd p1))
    <+>
    (map (fun p2' -> (seq,p1,p2')) (Shrink.list_elems Spec.shrink_cmd p2))

  let arb_cmds_par seq_len par_len =
    let gen_triple =
      Gen.(int_range 2 (2*par_len) >>= fun dbl_plen ->
           let seq_pref_gen = gen_cmds_size (int_bound seq_len) in
           let par_len1 = dbl_plen/2 in
           let par_gen1 = gen_cmds_size (return par_len1) in
           let par_gen2 = gen_cmds_size (return (dbl_plen - par_len1)) in
           triple seq_pref_gen par_gen1 par_gen2) in
    make ~print:(print_triple_vertical Spec.show_cmd) ~shrink:shrink_triple gen_triple

  let rec check_seq_cons pref cs1 cs2 seq_sut seq_trace = match pref with
    | (c,res)::pref' ->
        if Spec.equal_res res (Spec.run c seq_sut)
        then check_seq_cons pref' cs1 cs2 seq_sut (c::seq_trace)
        else (Spec.cleanup seq_sut; false)
    (* Invariant: call Spec.cleanup immediately after mismatch  *)
    | [] -> match cs1,cs2 with
            | [],[] -> Spec.cleanup seq_sut; true
            | [],(c2,res2)::cs2' ->
                if Spec.equal_res res2 (Spec.run c2 seq_sut)
                then check_seq_cons pref cs1 cs2' seq_sut (c2::seq_trace)
                else (Spec.cleanup seq_sut; false)
            | (c1,res1)::cs1',[] ->
                if Spec.equal_res res1 (Spec.run c1 seq_sut)
                then check_seq_cons pref cs1' cs2 seq_sut (c1::seq_trace)
                else (Spec.cleanup seq_sut; false)
            | (c1,res1)::cs1',(c2,res2)::cs2' ->
                (if Spec.equal_res res1 (Spec.run c1 seq_sut)
                 then check_seq_cons pref cs1' cs2 seq_sut (c1::seq_trace)
                 else (Spec.cleanup seq_sut; false))
                ||
                (* rerun to get seq_sut to same cmd branching point *)
                (let seq_sut' = Spec.init () in
                 let _ = interp_plain seq_sut' (List.rev seq_trace) in
                 if Spec.equal_res res2 (Spec.run c2 seq_sut')
                 then check_seq_cons pref cs1 cs2' seq_sut' (c2::seq_trace)
                 else (Spec.cleanup seq_sut'; false))

  (* Linearizability test *)
  let lin_test ~rep_count ~count ~retries ~name ~lin_prop =
    let arb_cmd_triple = arb_cmds_par 20 12 in
    Test.make ~count ~retries ~name
      arb_cmd_triple (repeat rep_count lin_prop)

  (* Negative linearizability test *)
  let neg_lin_test ~rep_count ~count ~retries ~name ~lin_prop =
    let arb_cmd_triple = arb_cmds_par 20 12 in
    Test.make_neg ~count ~retries ~name
      arb_cmd_triple (repeat rep_count lin_prop)
end
