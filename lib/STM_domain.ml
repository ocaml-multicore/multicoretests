open STM_base

module Make (Spec: STM_spec.Spec) = struct

  open Util
  open QCheck
  open STM_internal.Make(Spec)

  let arb_cmds_par = arb_cmds_par
  let arb_triple = arb_triple

  (* operate over arrays to avoid needless allocation underway *)
  let interp_sut_res sut cs =
    let cs_arr = Array.of_list cs in
    let res_arr = Array.map (fun c -> Domain.cpu_relax(); Spec.run c sut) cs_arr in
    List.combine cs (Array.to_list res_arr)

  (* Parallel agreement property based on [Domain] *)
  let agree_prop_par (seq_pref,cmds1,cmds2) =
    assume (all_interleavings_ok seq_pref cmds1 cmds2 Spec.init_state);
    let sut = Spec.init_sut () in
    let pref_obs = interp_sut_res sut seq_pref in
    let wait = Atomic.make true in
    let dom1 = Domain.spawn (fun () -> while Atomic.get wait do Domain.cpu_relax() done; try Ok (interp_sut_res sut cmds1) with exn -> Error exn) in
    let dom2 = Domain.spawn (fun () -> Atomic.set wait false; try Ok (interp_sut_res sut cmds2) with exn -> Error exn) in
    let obs1 = Domain.join dom1 in
    let obs2 = Domain.join dom2 in
    let obs1 = match obs1 with Ok v -> v | Error exn -> raise exn in
    let obs2 = match obs2 with Ok v -> v | Error exn -> raise exn in
    let ()   = Spec.cleanup sut in
    check_obs pref_obs obs1 obs2 Spec.init_state
      || Test.fail_reportf "  Results incompatible with linearized model\n\n%s"
         @@ print_triple_vertical ~fig_indent:5 ~res_width:35
           (fun (c,r) -> Printf.sprintf "%s : %s" (Spec.show_cmd c) (STM_spec.show_res r))
           (pref_obs,obs1,obs2)

  let agree_test_par ~count ~name =
    let rep_count = 25 in
    let seq_len,par_len = 20,12 in
    let max_gen = 3*count in (* precond filtering may require extra generation: max. 3*count though *)
    Test.make ~retries:15 ~max_gen ~count ~name
      (arb_cmds_par seq_len par_len)
      (repeat rep_count agree_prop_par) (* 25 times each, then 25 * 15 times when shrinking *)

  let neg_agree_test_par ~count ~name =
    let rep_count = 25 in
    let seq_len,par_len = 20,12 in
    let max_gen = 3*count in (* precond filtering may require extra generation: max. 3*count though *)
    Test.make_neg ~retries:15 ~max_gen ~count ~name
      (arb_cmds_par seq_len par_len)
      (repeat rep_count agree_prop_par) (* 25 times each, then 25 * 15 times when shrinking *)


  end
