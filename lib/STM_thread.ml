open STM

module MakeExt (Spec: SpecExt) = struct

  open Util
  open QCheck
  open Internal.Make(Spec)
    [@alert "-internal"]

  exception ThreadNotFinished

  let arb_cmds_triple = arb_cmds_triple

  let alloc_callback _src =
    Thread.yield ();
    None

  let yield_tracker =
    Gc.Memprof.{ null_tracker with alloc_minor = alloc_callback;
                                   alloc_major = alloc_callback; }

  (* [interp_sut_res] specialized for [Threads] *)
  let rec interp_sut_res sut cs = match cs with
    | [] -> []
    | c::cs ->
       Thread.yield ();
       let res = Spec.run c sut in
       (c,res)::interp_sut_res sut cs

  (* Concurrent agreement property based on [Threads] *)
  let agree_prop_conc (seq_pref,cmds1,cmds2) =
    let sut = Spec.init_sut () in
    let obs1,obs2 = ref (Error ThreadNotFinished), ref (Error ThreadNotFinished) in
    (* Gc.Memprof.{start,stop} raises Failure on OCaml 5.0 and 5.1 *)
    (try ignore (Gc.Memprof.start ~sampling_rate:1e-1 ~callstack_size:0 yield_tracker) with Failure _ -> ());
    let pref_obs = Spec.wrap_cmd_seq @@ fun () -> interp_sut_res sut seq_pref in
    let wait = ref true in
    let th1 = Thread.create (fun () -> Spec.wrap_cmd_seq @@ fun () -> while !wait do Thread.yield () done; obs1 := try Ok (interp_sut_res sut cmds1) with exn -> Error exn) () in
    let th2 = Thread.create (fun () -> Spec.wrap_cmd_seq @@ fun () -> wait := false; obs2 := try Ok (interp_sut_res sut cmds2) with exn -> Error exn) () in
    Thread.join th1;
    Thread.join th2;
    (try Gc.Memprof.stop () with Failure _ -> ());
    Spec.cleanup sut;
    let obs1 = match !obs1 with Ok v -> v | Error exn -> raise exn in
    let obs2 = match !obs2 with Ok v -> v | Error exn -> raise exn in
    check_obs pref_obs obs1 obs2 Spec.init_state
      || Test.fail_reportf "  Results incompatible with linearized model\n\n%s"
         @@ print_triple_vertical ~fig_indent:5 ~res_width:35
           (fun (c,r) -> Printf.sprintf "%s : %s" (Spec.show_cmd c) (show_res r))
           (pref_obs,obs1,obs2)

  (* Common magic constants *)
  let rep_count = 3 (* No. of repetitions of the non-deterministic property *)
  let retries = 25  (* Additional factor of repetition during shrinking *)
  let seq_len = 20  (* max length of the sequential prefix *)
  let par_len = 12  (* max length of the parallel cmd lists *)

  let agree_test_conc ~count ~name =
    (* a bigger [rep_count] for [Threads] as it is more difficult to trigger a problem *)
    let max_gen = 3*count in (* precond filtering may require extra generation: max. 3*count though *)
    Test.make ~retries ~max_gen ~count ~name
      (arb_cmds_triple seq_len par_len)
      (fun ((seq_pref,cmds1,cmds2) as triple) ->
         assume (all_interleavings_ok seq_pref cmds1 cmds2 Spec.init_state);
         repeat rep_count agree_prop_conc triple) (* 100 times each, then 100 * 15 times when shrinking *)

  let neg_agree_test_conc ~count ~name =
    let max_gen = 3*count in (* precond filtering may require extra generation: max. 3*count though *)
    Test.make_neg ~retries ~max_gen ~count ~name
      (arb_cmds_triple seq_len par_len)
      (fun ((seq_pref,cmds1,cmds2) as triple) ->
         assume (all_interleavings_ok seq_pref cmds1 cmds2 Spec.init_state);
         repeat rep_count agree_prop_conc triple) (* 100 times each, then 100 * 15 times when shrinking *)
end

module Make (Spec: Spec) =
  MakeExt (struct
    include SpecDefaults
    include Spec
  end)
