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

    | `Thread ->
        let arb_cmd_triple = arb_cmds_par seq_len par_len in
        let rep_count = 100 in
        Test.make ~count ~retries:5 ~name
          arb_cmd_triple (repeat rep_count lin_prop_thread)

    | `Thread ->
        let arb_cmd_triple = arb_cmds_par seq_len par_len in
        let rep_count = 100 in
        Test.make_neg ~count ~retries:5 ~name
          arb_cmd_triple (repeat rep_count lin_prop_thread)
