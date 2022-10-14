open QCheck
include Util

module Var : sig
  type t = int
  val next : unit -> t
  val reset : unit -> unit
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val shrink : t Shrink.t
end =
struct
  type t = int
  let next, reset =
    let counter = ref 0 in
    (fun () -> let old = !counter in
      incr counter; old),
    (fun () -> counter := 0)
  let show v = Format.sprintf "t%i" v
  let pp fmt v = Format.fprintf fmt "%s" (show v)
  let shrink = Shrink.int
end

module Env : sig
  type t = Var.t list
  val gen_t_var : t -> Var.t Gen.t
  val valid_t_vars : t -> Var.t -> Var.t Iter.t
end =
struct
  type t = Var.t list
  let gen_t_var env = Gen.oneofl env
  let valid_t_vars env v =
    match List.filter (fun v' -> v' <= v) env with
    | v' :: _ when v' = v -> Iter.return v
    | env'                ->
      let a = Array.of_list env' in
      let length = Array.length a in
      Iter.map (fun i -> a.(length - i - 1)) (Shrink.int length)
end

module type CmdSpec = sig
  type t
  (** The type of the system under test *)

  type cmd
  (** The type of commands *)

  val show_cmd : cmd -> string
  (** [show_cmd c] returns a string representing the command [c]. *)

  val gen_cmd : Var.t Gen.t -> (Var.t option * cmd) Gen.t
  (** A command generator.
      It accepts a variable generator and generates a pair [(opt,cmd)] with the option indicating
      an storage index to store the [cmd]'s result. *)

  val shrink_cmd : cmd Shrink.t
  (** A command shrinker.
      To a first approximation you can use [Shrink.nil]. *)

  val fix_cmd : Env.t -> cmd -> cmd Iter.t
  (** A command fixer.
      Fixes the given [cmd] so that it uses only states in the given
      [Env.t]. If the initial [cmd] used a state that is no longer
      available, it should iterate over all the lesser states
      available in [Env.t]. If all the necessary states are still
      available, it should generate a one-element iterator.
      Can assume that [Env.t] is sorted in decreasing order. *)

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

  val run : (Var.t option * cmd) -> t array -> res
  (** [run (opt,c) t] should interpret the command [c] over the various instances of the system under test [t array] (typically side-effecting).
      [opt] indicates the index to store the result. *)
end

(** A functor to create Domain and Thread test setups.
    We use it below, but it can also be used independently *)
module MakeDomThr(Spec : CmdSpec)
  = struct

  (* plain interpreter of a cmd list *)
  let interp_plain sut cs = List.map (fun c -> (c, Spec.run c sut)) cs

  (* operate over arrays to avoid needless allocation underway *)
  let interp sut cs =
    let cs_arr = Array.of_list cs in
    let res_arr = Array.map (fun c -> Domain.cpu_relax(); Spec.run c sut) cs_arr in
    List.combine cs (Array.to_list res_arr)

  (* Note: On purpose we use
     - a non-tail-recursive function and
     - an (explicit) allocation in the loop body
     since both trigger statistically significant more thread issues/interleaving *)
  let rec interp_thread sut cs = match cs with
    | [] -> []
    | c::cs ->
        Thread.yield ();
        let res = Spec.run c sut in
        (c,res)::interp_thread sut cs

  (* val gen_cmds : Env.t -> int -> (Env.t * (Var.t option * Spec.cmd) list) Gen.t *)
  let rec gen_cmds env fuel =
    Gen.(if fuel = 0
         then return (env,[])
         else
           Spec.gen_cmd (Env.gen_t_var env) >>= fun (opt,c) ->
             let env = match opt with None -> env | Some v -> v::env in
             gen_cmds env (fuel-1) >>= fun (env,cs) -> return (env,(opt,c)::cs))
  (** A fueled command list generator.
      Accepts an environment parameter [env] to enable [cmd] generation of multiple [t]s
      and returns the extended environment. *)

  let gen_cmds_size env size_gen = Gen.sized_size size_gen (gen_cmds env)

  let shrink_cmd (opt,c) = Iter.map (fun c -> opt,c) (Spec.shrink_cmd c)

  (* Note: the [env] fed to [Spec.fix_cmd] are in reverse (ie should
     be _decreasing_) order *)
  let fix_cmds env cmds =
    let rec aux env cmds =
      match cmds with
      | [] -> Iter.return []
      | (opt,cmd) :: cmds ->
          let env' = Option.fold ~none:env ~some:(fun i -> i::env) opt in
          Iter.map2 (fun cmd cmds -> (opt,cmd)::cmds) (Spec.fix_cmd env cmd) (aux env' cmds)
    in aux env cmds

  (* Note that the result is built in reverse (ie should be
     _decreasing_) order *)
  let rec extract_env env cmds =
    match cmds with
    | []                  -> env
    | (Some i, _) :: cmds -> extract_env (i::env) cmds
    | (None,   _) :: cmds -> extract_env     env  cmds

  let shrink_triple' (seq,p1,p2) =
    let open Iter in
    (* Shrinking heuristic:
       First reduce the cmd list sizes as much as possible, since the interleaving
       is most costly over long cmd lists. *)
    let concat_map f it = flatten (map f it) in
    let fix_seq seq =
      let seq_env = extract_env [0] seq in
      let triple seq p1 p2 = (seq,p1,p2) in
      map triple (fix_cmds [0] seq) <*> fix_cmds seq_env p1 <*> fix_cmds seq_env p2
    in
    let seq_env = extract_env [0] seq in

    concat_map fix_seq (Shrink.list_spine seq)
    <+>
    (match p1 with [] -> Iter.empty | c1::c1s -> Iter.return (seq@[c1],c1s,p2))
    <+>
    (match p2 with [] -> Iter.empty | c2::c2s -> Iter.return (seq@[c2],p1,c2s))
    <+>
    concat_map (fun p1 -> Iter.map (fun p1 -> (seq,p1,p2)) (fix_cmds seq_env p1)) (Shrink.list_spine p1)
    <+>
    concat_map (fun p2 -> Iter.map (fun p2 -> (seq,p1,p2)) (fix_cmds seq_env p2)) (Shrink.list_spine p2)
    <+>
    (* Secondly reduce the cmd data of individual list elements *)
    (map (fun seq' -> (seq',p1,p2)) (Shrink.list_elems shrink_cmd seq))
    <+>
    (map (fun p1' -> (seq,p1',p2)) (Shrink.list_elems shrink_cmd p1))
    <+>
    (map (fun p2' -> (seq,p1,p2')) (Shrink.list_elems shrink_cmd p2))

  let shrink_triple (size,t) =
    Iter.map (fun t -> (size,t)) (shrink_triple' t)

  let show_cmd (opt,c) = match opt with
    | None   -> Spec.show_cmd c
    | Some v -> Printf.sprintf "let %s = %s" (Var.show v) (Spec.show_cmd c)

  let init_cmd = "let t0 = init ()"
  let init_cmd_ret = init_cmd ^ "  : ()"

  let arb_cmds_par seq_len par_len =
    let gen_triple st =
      Var.reset ();
      let init_var = Var.next () in
      assert (init_var = 0);
      Gen.(int_range 2 (2*par_len) >>= fun dbl_plen ->
           let par_len1 = dbl_plen/2 in
           gen_cmds_size [init_var] (int_bound seq_len) >>= fun (env,seq_pref) ->
           gen_cmds_size env (return par_len1) >>= fun (_env1,par1) ->
           gen_cmds_size env (return (dbl_plen - par_len1)) >>= fun (_env2,par2) ->
           let array_size = Var.next () in
           return (array_size,(seq_pref,par1,par2))) st
    in
    make ~print:(fun (_,t) -> print_triple_vertical ~init_cmd show_cmd t) ~shrink:shrink_triple gen_triple

  let init_sut array_size =
    let sut = Spec.init () in
    Array.make array_size sut

  let cleanup sut seq_pref cmds1 cmds2 =
    let cleanup_opt ((opt,_c),_res) = match opt with
      | None -> ()
      | Some v -> Spec.cleanup sut.(v) in
    Spec.cleanup sut.(0); (* always present *)
    List.iter cleanup_opt seq_pref;
    List.iter cleanup_opt cmds1;
    List.iter cleanup_opt cmds2

  let rec check_seq_cons array_size pref cs1 cs2 seq_sut seq_trace = match pref with
    | (c,res)::pref' ->
        if Spec.equal_res res (Spec.run c seq_sut)
        then check_seq_cons array_size pref' cs1 cs2 seq_sut (c::seq_trace)
        else (cleanup seq_sut pref cs1 cs2; false)
    (* Invariant: call Spec.cleanup immediately after mismatch  *)
    | [] -> match cs1,cs2 with
            | [],[] -> cleanup seq_sut pref cs1 cs2; true
            | [],(c2,res2)::cs2' ->
                if Spec.equal_res res2 (Spec.run c2 seq_sut)
                then check_seq_cons array_size pref cs1 cs2' seq_sut (c2::seq_trace)
                else (cleanup seq_sut pref cs1 cs2; false)
            | (c1,res1)::cs1',[] ->
                if Spec.equal_res res1 (Spec.run c1 seq_sut)
                then check_seq_cons array_size pref cs1' cs2 seq_sut (c1::seq_trace)
                else (cleanup seq_sut pref cs1 cs2; false)
            | (c1,res1)::cs1',(c2,res2)::cs2' ->
                (if Spec.equal_res res1 (Spec.run c1 seq_sut)
                 then check_seq_cons array_size pref cs1' cs2 seq_sut (c1::seq_trace)
                 else (cleanup seq_sut pref cs1 cs2; false))
                ||
                (* rerun to get seq_sut to same cmd branching point *)
                (let seq_sut' = init_sut array_size in
                 let _ = interp_plain seq_sut' (List.rev seq_trace) in
                 if Spec.equal_res res2 (Spec.run c2 seq_sut')
                 then check_seq_cons array_size pref cs1 cs2' seq_sut' (c2::seq_trace)
                 else (cleanup seq_sut' pref cs1 cs2; false))

  (* Linearizability property based on [Domain] and an Atomic flag *)
  let lin_prop_domain (array_size, (seq_pref,cmds1,cmds2)) =
    let sut = init_sut array_size in
    let pref_obs = interp sut seq_pref in
    let wait = Atomic.make true in
    let dom1 = Domain.spawn (fun () -> while Atomic.get wait do Domain.cpu_relax() done; try Ok (interp sut cmds1) with exn -> Error exn) in
    let dom2 = Domain.spawn (fun () -> Atomic.set wait false; try Ok (interp sut cmds2) with exn -> Error exn) in
    let obs1 = Domain.join dom1 in
    let obs2 = Domain.join dom2 in
    let obs1 = match obs1 with Ok v -> v | Error exn -> raise exn in
    let obs2 = match obs2 with Ok v -> v | Error exn -> raise exn in
    cleanup sut pref_obs obs1 obs2;
    let seq_sut = init_sut array_size in
    check_seq_cons array_size pref_obs obs1 obs2 seq_sut []
      || Test.fail_reportf "  Results incompatible with sequential execution\n\n%s"
         @@ print_triple_vertical ~fig_indent:5 ~res_width:35 ~init_cmd:init_cmd_ret
              (fun (c,r) -> Printf.sprintf "%s : %s" (show_cmd c) (Spec.show_res r))
              (pref_obs,obs1,obs2)

  (* Linearizability property based on [Thread] *)
  let lin_prop_thread =
    (fun (array_size, (seq_pref, cmds1, cmds2)) ->
      let sut = init_sut array_size in
      let obs1, obs2 = ref [], ref [] in
      let pref_obs = interp_plain sut seq_pref in
      let wait = ref true in
      let th1 = Thread.create (fun () -> while !wait do Thread.yield () done; obs1 := interp_thread sut cmds1) () in
      let th2 = Thread.create (fun () -> wait := false; obs2 := interp_thread sut cmds2) () in
      Thread.join th1;
      Thread.join th2;
      cleanup sut pref_obs !obs1 !obs2;
      let seq_sut = init_sut array_size in
      (* we reuse [check_seq_cons] to linearize and interpret sequentially *)
      check_seq_cons array_size pref_obs !obs1 !obs2 seq_sut []
      || Test.fail_reportf "  Results incompatible with sequential execution\n\n%s"
         @@ print_triple_vertical ~fig_indent:5 ~res_width:35 ~init_cmd:init_cmd_ret
              (fun (c,r) -> Printf.sprintf "%s : %s" (show_cmd c) (Spec.show_res r))
              (pref_obs,!obs1,!obs2))
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


(** A functor to create all three (Domain, Thread, and Effect) test setups.
    The result [include]s the output module from the [MakeDomThr] functor above *)
module Make(Spec : CmdSpec)
= struct

  (** A refined [CmdSpec] specification with generator-controlled [Yield] effects *)
  module EffSpec
  = struct

    type t = Spec.t
    let init = Spec.init
    let cleanup = Spec.cleanup

    type cmd = SchedYield | UserCmd of Spec.cmd

    let show_cmd c = match c with
      | SchedYield -> "<SchedYield>"
      | UserCmd c  -> Spec.show_cmd c

    let gen_cmd env =
      (Gen.frequency
         [(3,Gen.return (None,SchedYield));
          (5,Gen.map (fun (opt,c) -> (opt,UserCmd c)) (Spec.gen_cmd env))])

    let fix_cmd env = function
      | SchedYield -> Iter.return SchedYield
      | UserCmd cmd -> Iter.map (fun c -> UserCmd c) (Spec.fix_cmd env cmd)

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
      | _, SchedYield ->
          (yield (); SchedYieldRes)
      | opt, UserCmd uc ->
          let res = Spec.run (opt,uc) sut in
          UserRes res
  end

  module EffTest = MakeDomThr(EffSpec)

  let filter_res rs = List.filter (fun ((_,c),_) -> c <> EffSpec.SchedYield) rs

  (* Parallel agreement property based on effect-handler scheduler *)
  let lin_prop_effect =
    (fun (array_size,(seq_pref,cmds1,cmds2)) ->
       let sut = EffTest.init_sut array_size in
       (* exclude [Yield]s from sequential prefix *)
       let pref_obs = EffTest.interp_plain sut (List.filter (fun (_,c) -> c <> EffSpec.SchedYield) seq_pref) in
       let obs1,obs2 = ref [], ref [] in
       let main () =
         (* For now, we reuse [interp_thread] which performs useless [Thread.yield] on single-domain/fibered program *)
         fork (fun () -> let tmp1 = EffTest.interp_thread sut cmds1 in obs1 := tmp1);
         fork (fun () -> let tmp2 = EffTest.interp_thread sut cmds2 in obs2 := tmp2); in
       let () = start_sched main in
       let () = EffTest.cleanup sut pref_obs !obs1 !obs2 in
       let seq_sut = EffTest.init_sut array_size in
       (* exclude [Yield]s from sequential executions when searching for an interleaving *)
       EffTest.check_seq_cons array_size (filter_res pref_obs) (filter_res !obs1) (filter_res !obs2) seq_sut []
       || Test.fail_reportf "  Results incompatible with linearized model\n\n%s"
       @@ Util.print_triple_vertical ~fig_indent:5 ~res_width:35 ~init_cmd:EffTest.init_cmd_ret
         (fun (c,r) -> Printf.sprintf "%s : %s" (EffTest.show_cmd c) (EffSpec.show_res r))
         (pref_obs,!obs1,!obs2))

  module FirstTwo = MakeDomThr(Spec)
  include FirstTwo

  (* Linearizability test based on [Domain], [Thread], or [Effect] *)
  let lin_test ~count ~name (lib : [ `Domain | `Thread | `Effect ]) =
    let seq_len,par_len = 20,12 in
    match lib with
    | `Domain ->
        let arb_cmd_triple = arb_cmds_par seq_len par_len in
        let rep_count = 50 in
        Test.make ~count ~retries:3 ~name
          arb_cmd_triple (repeat rep_count lin_prop_domain)
    | `Thread ->
        let arb_cmd_triple = arb_cmds_par seq_len par_len in
        let rep_count = 100 in
        Test.make ~count ~retries:5 ~name
          arb_cmd_triple (repeat rep_count lin_prop_thread)
    | `Effect ->
        (* this generator is over [EffSpec.cmd] including [SchedYield], not [Spec.cmd] like the above two *)
        let arb_cmd_triple = EffTest.arb_cmds_par seq_len par_len in
        let rep_count = 1 in
        Test.make ~count ~retries:10 ~name
          arb_cmd_triple (repeat rep_count lin_prop_effect)

  (* Negative linearizability test based on [Domain], [Thread], or [Effect] *)
  let neg_lin_test ~count ~name (lib : [ `Domain | `Thread | `Effect ]) =
    let seq_len,par_len = 20,12 in
    match lib with
    | `Domain ->
        let arb_cmd_triple = arb_cmds_par seq_len par_len in
        let rep_count = 50 in
        Test.make_neg ~count ~retries:3 ~name
          arb_cmd_triple (repeat rep_count lin_prop_domain)
    | `Thread ->
        let arb_cmd_triple = arb_cmds_par seq_len par_len in
        let rep_count = 100 in
        Test.make_neg ~count ~retries:5 ~name
          arb_cmd_triple (repeat rep_count lin_prop_thread)
    | `Effect ->
        (* this generator is over [EffSpec.cmd] including [SchedYield], not [Spec.cmd] like the above two *)
        let arb_cmd_triple = EffTest.arb_cmds_par seq_len par_len in
        let rep_count = 1 in
        Test.make_neg ~count ~retries:10 ~name
          arb_cmd_triple (repeat rep_count lin_prop_effect)
end
