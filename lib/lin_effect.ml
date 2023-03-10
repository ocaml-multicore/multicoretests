open Lin

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
        exnc = (fun e -> (Stdlib.print_string (Printexc.to_string e); raise e));
        effc = (fun (type a) (e : a t) -> match e with
            | Yield  -> Some (fun (k : (a, _) continuation) -> enqueue k q; dequeue q)
            | Fork f -> Some (fun (k : (a, _) continuation) -> enqueue k q; spawn f)
            | _      -> None ) }
  in
  spawn main

(* short hands *)
let fork f = perform (Fork f)
let yield () = perform Yield

module Make_internal (Spec : Internal.CmdSpec [@alert "-internal"]) = struct


  (** A refined [CmdSpec] specification with generator-controlled [Yield] effects *)
  module EffSpec
  = struct
    open QCheck

    type t = Spec.t
    let init = Spec.init
    let cleanup = Spec.cleanup

    type cmd = SchedYield | UserCmd of Spec.cmd [@@deriving qcheck]

    let show_cmd c = match c with
      | SchedYield -> "<SchedYield>"
      | UserCmd c  -> Spec.show_cmd c

    let gen_cmd env =
      (Gen.frequency
         [(3,Gen.return (None,SchedYield));
          (5,Gen.map (fun (opt,c) -> (opt,UserCmd c)) (Spec.gen_cmd env))])

    let shrink_cmd env c = match c with
      | SchedYield -> Iter.empty
      | UserCmd c -> Iter.map (fun c' -> UserCmd c') (Spec.shrink_cmd env c)

    let cmd_uses_var var c = match c with
      | SchedYield -> false
      | UserCmd cmd -> Spec.cmd_uses_var var cmd

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

  module EffTest = Internal.Make(EffSpec) [@alert "-internal"]

  let arb_cmds_triple = EffTest.arb_cmds_triple

  let filter_res rs = List.filter (fun ((_,c),_) -> c <> EffSpec.SchedYield) rs

  let rec interp sut cs = match cs with
    | [] -> []
    | c::cs ->
        let res = EffSpec.run c sut in
        (c,res)::interp sut cs

  (* Concurrent agreement property based on effect-handler scheduler *)
  let lin_prop { Internal.env_size=array_size; seq_prefix=seq_pref; tail_left=cmds1; tail_right=cmds2 } =
    let sut = EffTest.init_sut array_size in
    let pref_obs = EffTest.interp_plain sut (List.filter (fun (_,c) -> c <> EffSpec.SchedYield) seq_pref) in
    (* exclude [Yield]s from sequential prefix *)
    let obs1,obs2 = ref (Ok []), ref (Ok []) in
    let main () =
      fork (fun () -> let tmp1 = try Ok (interp sut cmds1) with exn -> Error exn in obs1 := tmp1);
      fork (fun () -> let tmp2 = try Ok (interp sut cmds2) with exn -> Error exn in obs2 := tmp2); in
    let () = start_sched main in
    let () = EffTest.cleanup sut seq_pref cmds1 cmds2 in
    let obs1 = match !obs1 with Ok v -> ref v | Error exn -> raise exn in
    let obs2 = match !obs2 with Ok v -> ref v | Error exn -> raise exn in
    let seq_sut = EffTest.init_sut array_size in
    (* exclude [Yield]s from sequential executions when searching for an interleaving *)
    EffTest.check_seq_cons array_size (filter_res pref_obs) (filter_res !obs1) (filter_res !obs2) seq_sut []
    || QCheck.Test.fail_reportf "  Results incompatible with linearized model\n\n%s"
    @@ Util.print_triple_vertical ~fig_indent:5 ~res_width:35 ~init_cmd:EffTest.init_cmd_ret
      (fun (c,r) -> Printf.sprintf "%s : %s" (EffTest.show_cmd c) (EffSpec.show_res r))
      (pref_obs,!obs1,!obs2)[@@alert "-internal"]

  let lin_test ~count ~name =
    let arb_cmd_triple = EffTest.arb_cmds_triple 20 12 in
    let rep_count = 1 in
    QCheck.Test.make ~count ~retries:10 ~name
      arb_cmd_triple (Util.repeat rep_count lin_prop)

  let neg_lin_test ~count ~name =
    let arb_cmd_triple = EffTest.arb_cmds_triple 20 12 in
    let rep_count = 1 in
    QCheck.Test.make_neg ~count ~retries:10 ~name
      arb_cmd_triple (Util.repeat rep_count lin_prop)
end

module Make (Spec : Spec) = Make_internal(MakeCmd(Spec))
