open STM

module Make (Spec: Spec) = struct

  open Util
  open QCheck
  open Internal.Make(Spec)
    [@alert "-internal"]

  let check_obs = check_obs
  let all_interleavings_ok (seq_pref,cmds1,cmds2) =
    all_interleavings_ok seq_pref cmds1 cmds2 Spec.init_state
  let arb_cmds_triple = arb_cmds_triple
  let arb_triple = arb_triple
  let arb_triple_asym seq_len par_len arb0 arb1 arb2 =
    let arb_triple = arb_triple seq_len par_len arb0 arb1 arb2 in
    set_print (print_triple_vertical ~center_prefix:false Spec.show_cmd) arb_triple

  (* operate over arrays to avoid needless allocation underway *)
  let interp_sut_res sut cs =
    let cs_arr = Array.of_list cs in
    let res_arr = Array.map (fun c -> Domain.cpu_relax(); Spec.run c sut) cs_arr in
    List.combine cs (Array.to_list res_arr)

  let run_par seq_pref cmds1 cmds2 =
    let sut = Spec.init_sut () in
    let pref_obs = interp_sut_res sut seq_pref in
    let wait = Atomic.make true in
    let dom1 = Domain.spawn (fun () -> while Atomic.get wait do Domain.cpu_relax() done; try Ok (interp_sut_res sut cmds1) with exn -> Error exn) in
    let dom2 = Domain.spawn (fun () -> Atomic.set wait false; try Ok (interp_sut_res sut cmds2) with exn -> Error exn) in
    let obs1 = Domain.join dom1 in
    let obs2 = Domain.join dom2 in
    let ()   = Spec.cleanup sut in
    let obs1 = match obs1 with Ok v -> v | Error exn -> raise exn in
    let obs2 = match obs2 with Ok v -> v | Error exn -> raise exn in
    pref_obs, obs1, obs2

  let agree_prop_par (seq_pref,cmds1,cmds2) =
    let pref_obs, obs1, obs2 = run_par seq_pref cmds1 cmds2 in
    check_obs pref_obs obs1 obs2 Spec.init_state
      || Test.fail_reportf "  Results incompatible with linearized model\n\n%s"
         @@ print_triple_vertical ~fig_indent:5 ~res_width:35
           (fun (c,r) -> Printf.sprintf "%s : %s" (Spec.show_cmd c) (show_res r))
           (pref_obs,obs1,obs2)

  let stress_prop_par (seq_pref,cmds1,cmds2) =
    let _ = run_par seq_pref cmds1 cmds2 in
    true

  let agree_prop_par_asym (seq_pref, cmds1, cmds2) =
    let sut = Spec.init_sut () in
    let pref_obs = interp_sut_res sut seq_pref in
    let wait = Atomic.make 2 in
    let child_dom =
      Domain.spawn (fun () ->
          Atomic.decr wait;
          while Atomic.get wait <> 0 do Domain.cpu_relax() done;
          try Ok (interp_sut_res sut cmds2) with exn -> Error exn)
    in
    Atomic.decr wait;
    while Atomic.get wait <> 0 do Domain.cpu_relax() done;
    let parent_obs = try Ok (interp_sut_res sut cmds1) with exn -> Error exn in
    let child_obs = Domain.join child_dom in
    let () = Spec.cleanup sut in
    let parent_obs = match parent_obs with Ok v -> v | Error exn -> raise exn in
    let child_obs = match child_obs with Ok v -> v | Error exn -> raise exn in
    check_obs pref_obs parent_obs child_obs Spec.init_state
      || Test.fail_reportf "  Results incompatible with linearized model:\n\n%s"
         @@ print_triple_vertical ~fig_indent:5 ~res_width:35 ~center_prefix:false
           (fun (c,r) -> Printf.sprintf "%s : %s" (Spec.show_cmd c) (show_res r))
           (pref_obs,parent_obs,child_obs)

  let agree_test_par ~count ~name =
    let rep_count = 25 in
    let seq_len,par_len = 20,12 in
    let max_gen = 3*count in (* precond filtering may require extra generation: max. 3*count though *)
    Test.make ~retries:10 ~max_gen ~count ~name
      (arb_cmds_triple seq_len par_len)
      (fun triple ->
         assume (all_interleavings_ok triple);
         repeat rep_count agree_prop_par triple) (* 25 times each, then 25 * 10 times when shrinking *)

  let stress_test_par ~count ~name =
    let rep_count = 25 in
    let seq_len,par_len = 20,12 in
    let max_gen = 3*count in (* precond filtering may require extra generation: max. 3*count though *)
    Test.make ~retries:10 ~max_gen ~count ~name
      (arb_cmds_triple seq_len par_len)
      (fun triple ->
         assume (all_interleavings_ok triple);
         repeat rep_count stress_prop_par triple) (* 25 times each, then 25 * 10 times when shrinking *)

  let neg_agree_test_par ~count ~name =
    let rep_count = 25 in
    let seq_len,par_len = 20,12 in
    let max_gen = 3*count in (* precond filtering may require extra generation: max. 3*count though *)
    Test.make_neg ~retries:10 ~max_gen ~count ~name
      (arb_cmds_triple seq_len par_len)
      (fun triple ->
         assume (all_interleavings_ok triple);
         repeat rep_count agree_prop_par triple) (* 25 times each, then 25 * 10 times when shrinking *)

  let agree_test_par_asym ~count ~name =
    let rep_count = 25 in
    let seq_len,par_len = 20,12 in
    let max_gen = 3*count in (* precond filtering may require extra generation: max. 3*count though *)
    Test.make ~retries:10 ~max_gen ~count ~name
      (arb_cmds_triple seq_len par_len)
      (fun triple ->
         assume (all_interleavings_ok triple);
         repeat rep_count agree_prop_par_asym triple) (* 25 times each, then 25 * 10 times when shrinking *)

  let neg_agree_test_par_asym ~count ~name =
    let rep_count = 25 in
    let seq_len,par_len = 20,12 in
    let max_gen = 3*count in (* precond filtering may require extra generation: max. 3*count though *)
    Test.make_neg ~retries:10 ~max_gen ~count ~name
      (arb_cmds_triple seq_len par_len)
      (fun triple ->
         assume (all_interleavings_ok triple);
         repeat rep_count agree_prop_par_asym triple) (* 25 times each, then 25 * 10 times when shrinking *)
end
