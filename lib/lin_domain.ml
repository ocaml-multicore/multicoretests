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

    | `Domain ->
        let arb_cmd_triple = arb_cmds_par seq_len par_len in
        let rep_count = 50 in
        Test.make ~count ~retries:3 ~name
          arb_cmd_triple (repeat rep_count lin_prop_domain)

    | `Domain ->
        let arb_cmd_triple = arb_cmds_par seq_len par_len in
        let rep_count = 50 in
        Test.make_neg ~count ~retries:3 ~name
          arb_cmd_triple (repeat rep_count lin_prop_domain)
