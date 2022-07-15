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

module Common(Spec : CmdSpec)
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

end


(** A functor to create Domain and Thread test setups.
    We use it below, but it can also be used independently *)
module MakeDom(Spec : CmdSpec)
  = struct

  include Common(Spec)

  (* operate over arrays to avoid needless allocation underway *)
  let interp sut cs =
    let cs_arr = Array.of_list cs in
    let res_arr = Array.map (fun c -> Domain.cpu_relax(); Spec.run c sut) cs_arr in
    List.combine cs (Array.to_list res_arr)

  (* Linearizability property based on [Domain] and an Atomic flag *)
  let lin_prop_domain (seq_pref,cmds1,cmds2) =
    let sut = Spec.init () in
    let pref_obs = interp sut seq_pref in
    let wait = Atomic.make true in
    let dom1 = Domain.spawn (fun () -> while Atomic.get wait do Domain.cpu_relax() done; try Ok (interp sut cmds1) with exn -> Error exn) in
    let dom2 = Domain.spawn (fun () -> Atomic.set wait false; try Ok (interp sut cmds2) with exn -> Error exn) in
    let obs1 = Domain.join dom1 in
    let obs2 = Domain.join dom2 in
    let obs1 = match obs1 with Ok v -> v | Error exn -> raise exn in
    let obs2 = match obs2 with Ok v -> v | Error exn -> raise exn in
    let seq_sut = Spec.init () in
    check_seq_cons pref_obs obs1 obs2 seq_sut []
      || Test.fail_reportf "  Results incompatible with sequential execution\n\n%s"
         @@ print_triple_vertical ~fig_indent:5 ~res_width:35
              (fun (c,r) -> Printf.sprintf "%s : %s" (Spec.show_cmd c) (Spec.show_res r))
              (pref_obs,obs1,obs2)
end

module AddYield(Spec : CmdSpec) : CmdSpec = struct
  type t = Spec.t
  type cmd =
    | UserCmd of Spec.cmd
    | Yield
  let usercmd c = UserCmd c

  let show_cmd = function
    | UserCmd cmd -> Spec.show_cmd cmd
    | Yield -> "<Thread.yield>"

  let gen_cmd = Gen.frequency
                  [(3, Gen.return Yield);
                   (5, Gen.map usercmd Spec.gen_cmd)]
  let shrink_cmd = function
    | UserCmd cmd -> Iter.map usercmd (Spec.shrink_cmd cmd)
    | Yield -> Iter.empty

  type res =
    | UserRes of Spec.res
    | YieldRes

  let show_res = function
    | UserRes res -> Spec.show_res res
    | YieldRes -> "()"
  let userres r = UserRes r

  let equal_res r0 r1 =
    match r0, r1 with
      UserRes r0, UserRes r1 -> Spec.equal_res r0 r1
    | YieldRes, YieldRes -> true
    | _,_ -> false

  let init = Spec.init
  let cleanup _ = ()

  let run cmd t =
    match cmd with
    | UserCmd cmd -> Spec.run cmd t |> userres
    | Yield -> Thread.yield (); YieldRes

end

module MakeThread (Spec: CmdSpec) = struct
  module Spec = AddYield(Spec)
  include Common(Spec)

  (* Note: On purpose we use
     - a non-tail-recursive function and
     - an (explicit) allocation in the loop body
     since both trigger statistically significant more thread issues/interleaving *)
  let rec interp_thread sut cs = match cs with
    | [] -> []
    | c::cs ->
        let res = Spec.run c sut in
        (c,res)::interp_thread sut cs
  (* Linearizability property based on [Thread] *)

  let lin_prop_thread =
    (fun (seq_pref, cmds1, cmds2) ->
      let sut = Spec.init () in
      let obs1, obs2 = ref [], ref [] in
      let pref_obs = interp_plain sut seq_pref in
      let wait = ref true in
      let th1 = Thread.create (fun () -> while !wait do Thread.yield () done; obs1 := interp_thread sut cmds1) () in
      let th2 = Thread.create (fun () -> wait := false; obs2 := interp_thread sut cmds2) () in
      Thread.join th1;
      Thread.join th2;
      Spec.cleanup sut;
      let seq_sut = Spec.init () in
      (* we reuse [check_seq_cons] to linearize and interpret sequentially *)
      check_seq_cons pref_obs !obs1 !obs2 seq_sut []
      || Test.fail_reportf "  Results incompatible with sequential execution\n\n%s"
         @@ print_triple_vertical ~fig_indent:5 ~res_width:35
              (fun (c,r) -> Printf.sprintf "%s : %s" (Spec.show_cmd c) (Spec.show_res r))
              (pref_obs,!obs1,!obs2))
end

module MakeDomThr(Spec : CmdSpec) =
  struct
    include MakeThread(Spec)
    include MakeDom(Spec)
  end


(** Definitions for Effect interpretation *)

(* Scheduler adapted from https://kcsrk.info/slides/retro_effects_simcorp.pdf *)
open Effect
open Effect.Deep

type _ t += Fork : (unit -> unit) -> unit t
         | Yield : unit t

let enqueue k q = Queue.push k q
let dequeue q =
  if Queue.is_empty q
  then () (*Finished*)
  else continue (Queue.pop q) ()

let start_sched main =
  (* scheduler's queue of continuations *)
  let q = Queue.create () in
  let rec spawn = fun (type res) (f : unit -> res) ->
    match_with f ()
      { retc = (fun _v -> dequeue q); (* value case *)
        exnc = (fun e -> print_string (Printexc.to_string e); raise e);
        effc = (fun (type a) (e : a t) -> match e with
            | Yield  -> Some (fun (k : (a, _) continuation) -> enqueue k q; dequeue q)
            | Fork f -> Some (fun (k : (a, _) continuation) -> enqueue k q; spawn f)
            | _      -> None ) }
  in
  spawn main

(* short hands *)
let fork f = perform (Fork f)
let yield () = perform Yield


(** A refined [CmdSpec] specification with generator-controlled [Yield] effects *)
module EffSpec(Spec : CmdSpec)
  = struct

  type t = Spec.t
  let init = Spec.init
  let cleanup = Spec.cleanup

  type cmd = SchedYield | UserCmd of Spec.cmd [@@deriving qcheck]

  let show_cmd c = match c with
    | SchedYield -> "<SchedYield>"
    | UserCmd c  -> Spec.show_cmd c

  let gen_cmd =
    (Gen.frequency
       [(3,Gen.return SchedYield);
        (5,Gen.map (fun c -> UserCmd c) Spec.gen_cmd)])

  let shrink_cmd c = match c with
    | SchedYield -> Iter.empty
    | UserCmd c -> Iter.map (fun c' -> UserCmd c') (Spec.shrink_cmd c)

  type res = SchedYieldRes | UserRes of Spec.res

  let show_res r = match r with
    | SchedYieldRes -> "<SchedYieldRes>"
    | UserRes r     -> Spec.show_res r

  let equal_res r r' = match r,r' with
    | SchedYieldRes, SchedYieldRes -> true
    | UserRes r, UserRes r' -> Spec.equal_res r r'
    | _, _ -> false

  let run c sut = match c with
    | SchedYield ->
       (yield (); SchedYieldRes)
    | UserCmd uc ->
       let res = Spec.run uc sut in
       UserRes res
end

module MakeEffect(Spec : CmdSpec)
  = struct
  module Spec = EffSpec(Spec)
  include Common (Spec)
  let filter_res rs = List.filter (fun (c,_) -> c <> Spec.SchedYield) rs
  let rec interp_effect sut cs = match cs with
    | [] -> []
    | c::cs ->
        let res = Spec.run c sut in
        (c,res)::interp_effect sut cs

   (* Parallel agreement property based on effect-handler scheduler *)
  let lin_prop_effect =
    (fun (seq_pref,cmds1,cmds2) ->
       let sut = Spec.init () in
       (* exclude [Yield]s from sequential prefix *)
       let pref_obs = interp_plain sut (List.filter (fun c -> c <> Spec.SchedYield) seq_pref) in
       let obs1,obs2 = ref [], ref [] in
       let main () =
         (* For now, we reuse [interp_thread] which performs useless [Thread.yield] on single-domain/fibered program *)
         fork (fun () -> let tmp1 = interp_effect sut cmds1 in obs1 := tmp1);
         fork (fun () -> let tmp2 = interp_effect sut cmds2 in obs2 := tmp2); in
       let () = start_sched main in
       let () = Spec.cleanup sut in
       let seq_sut = Spec.init () in
       (* exclude [Yield]s from sequential executions when searching for an interleaving *)
       check_seq_cons (filter_res pref_obs) (filter_res !obs1) (filter_res !obs2) seq_sut []
       || Test.fail_reportf "  Results incompatible with linearized model\n\n%s"
       @@ Util.print_triple_vertical ~fig_indent:5 ~res_width:35
         (fun (c,r) -> Printf.sprintf "%s : %s" (Spec.show_cmd c) (Spec.show_res r))
         (pref_obs,!obs1,!obs2))

  end

module Make(Spec: CmdSpec)
= struct

  (* for stats only -- remove when experiments are done *)
  module Thread = MakeThread(Spec)
  let lin_prop_thread = Thread.lin_prop_thread
  let arb_cmds_par = Thread.arb_cmds_par

  (* Linearizability test based on [Domain], [Thread], or [Effect] *)
  let lin_test ~count ~name (lib : [ `Domain | `Thread | `Effect ]) =
    let seq_len,par_len = 20,12 in
    match lib with
    | `Domain ->
       let open MakeDom(Spec) in
        let arb_cmd_triple = arb_cmds_par seq_len par_len in
        let rep_count = 50 in
        Test.make ~count ~retries:3 ~name:("Linearizable " ^ name ^ " with Domain")
          arb_cmd_triple (repeat rep_count lin_prop_domain)
    | `Thread ->
       let open MakeThread(Spec) in
        let arb_cmd_triple = arb_cmds_par seq_len par_len in
        let rep_count = 100 in
        Test.make ~count ~retries:5 ~name:("Linearizable " ^ name ^ " with Thread")
          arb_cmd_triple (repeat rep_count lin_prop_thread)
    | `Effect ->
       let open MakeEffect(Spec) in
        (* this generator is over [EffSpec.cmd] including [SchedYield], not [Spec.cmd] like the above two *)
        let arb_cmd_triple = arb_cmds_par seq_len par_len in
        let rep_count = 1 in
        Test.make ~count ~retries:10 ~name:("Linearizable " ^ name ^ " with Effect")
          arb_cmd_triple (repeat rep_count lin_prop_effect)

end
