open Lin

module Make_internal (Spec : Internal.CmdSpec [@alert "-internal"]) = struct
  module M = Internal.Make(Spec) [@alert "-internal"]
  include M

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

  let arb_cmds_triple = arb_cmds_triple

  (* Linearization property based on [Thread] *)
  let lin_prop =
    (fun (array_size, (seq_pref, cmds1, cmds2)) ->
      let sut = init_sut array_size in
      let obs1, obs2 = ref [], ref [] in
      let pref_obs = interp_plain sut seq_pref in
      let wait = ref true in
      let th1 = Thread.create (fun () -> while !wait do Thread.yield () done; obs1 := interp_thread sut cmds1) () in
      let th2 = Thread.create (fun () -> wait := false; obs2 := interp_thread sut cmds2) () in
      Thread.join th1;
      Thread.join th2;
      cleanup sut seq_pref cmds1 cmds2;
      let seq_sut = init_sut array_size in
      (* we reuse [check_seq_cons] to linearize and interpret sequentially *)
      check_seq_cons array_size pref_obs !obs1 !obs2 seq_sut []
      || QCheck.Test.fail_reportf "  Results incompatible with sequential execution\n\n%s"
         @@ Util.print_triple_vertical ~fig_indent:5 ~res_width:35 ~init_cmd:init_cmd_ret
              (fun (c,r) -> Printf.sprintf "%s : %s" (show_cmd c) (Spec.show_res r))
              (pref_obs,!obs1,!obs2))

  let lin_test ~count ~name =
    lin_test ~rep_count:100 ~count ~retries:5 ~name ~lin_prop:lin_prop

  let neg_lin_test ~count ~name =
    neg_lin_test ~rep_count:100 ~count ~retries:5 ~name ~lin_prop:lin_prop
end

module Make (Spec : Spec) = Make_internal(MakeCmd(Spec))
