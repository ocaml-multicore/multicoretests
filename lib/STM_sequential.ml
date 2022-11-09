open STM_base

module Make (Spec: STM_spec.Spec) = struct

  open QCheck
  open STM_internal.Make(Spec)

  (* re-export some functions *)
  let cmds_ok        = cmds_ok
  let check_obs      = check_obs
  let gen_cmds_size  = gen_cmds_size
  let shrink_triple  = shrink_triple

  (* operate over arrays to avoid needless allocation underway *)
  let interp_sut_res sut cs =
    let cs_arr = Array.of_list cs in
    let res_arr = Array.map (fun c -> Spec.run c sut) cs_arr in
    List.combine cs (Array.to_list res_arr)

  let print_seq_trace trace =
    List.fold_left
      (fun acc (c,r) -> Printf.sprintf "%s\n   %s : %s" acc (Spec.show_cmd c) (show_res r))
      "" trace

  let agree_prop =
    (fun cs ->
       assume (cmds_ok Spec.init_state cs);
       let sut = Spec.init_sut () in (* reset system's state *)
       let res = check_disagree Spec.init_state sut cs in
       let ()  = Spec.cleanup sut in
       match res with
       | None -> true
       | Some trace ->
           Test.fail_reportf "  Results incompatible with model\n%s"
           @@ print_seq_trace trace)

  let agree_test ~count ~name =
    Test.make ~name ~count (arb_cmds Spec.init_state) agree_prop

  let neg_agree_test ~count ~name =
    Test.make_neg ~name ~count (arb_cmds Spec.init_state) agree_prop

  end
