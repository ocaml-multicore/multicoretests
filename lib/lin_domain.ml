open Lin

module Make_internal (Spec : Internal.CmdSpec [@alert "-internal"]) = struct
  module M = Internal.Make(Spec) [@alert "-internal"]
  include M

  (* operate over arrays to avoid needless allocation underway *)
  let interp sut cs =
    let cs_arr = Array.of_list cs in
    let res_arr = Array.map (fun c -> Domain.cpu_relax(); Spec.run c sut) cs_arr in
    List.combine cs (Array.to_list res_arr)

  let run_parallel (seq_pref,cmds1,cmds2) =
    let sut = Spec.init () in
    let pref_obs = interp sut seq_pref in
    let barrier = Atomic.make 2 in
    let main cmds () =
      Atomic.decr barrier;
      while Atomic.get barrier <> 0 do Domain.cpu_relax () done;
      try Ok (interp sut cmds) with exn -> Error exn
    in
    let dom1 = Domain.spawn (main cmds1) in
    let dom2 = Domain.spawn (main cmds2) in
    let obs1 = Domain.join dom1 in
    let obs2 = Domain.join dom2 in
    Spec.cleanup sut ;
    let obs1 = match obs1 with Ok v -> v | Error exn -> raise exn in
    let obs2 = match obs2 with Ok v -> v | Error exn -> raise exn in
    (pref_obs,obs1,obs2)

  (* Linearization property based on [Domain] and an Atomic flag *)
  let lin_prop (seq_pref,cmds1,cmds2) =
    let pref_obs,obs1,obs2 = run_parallel (seq_pref,cmds1,cmds2) in
    let seq_sut = Spec.init () in
    check_seq_cons pref_obs obs1 obs2 seq_sut []
      || QCheck.Test.fail_reportf "  Results incompatible with sequential execution\n\n%s"
         @@ Util.print_triple_vertical ~fig_indent:5 ~res_width:35
              (fun (c,r) -> Printf.sprintf "%s : %s" (Spec.show_cmd c) (Spec.show_res r))
              (pref_obs,obs1,obs2)

  (* "Don't crash under parallel usage" property *)
  let stress_prop (seq_pref,cmds1,cmds2) =
    let _ = run_parallel (seq_pref,cmds1,cmds2) in
    true

  (* Common magic constants *)
  let rep_count = 50 (* No. of repetitions of the non-deterministic property *)
  let retries = 3    (* Additional factor of repetition during shrinking *)

  let lin_test ~count ~name =
    M.lin_test ~rep_count ~count ~retries ~name ~lin_prop

  let neg_lin_test ~count ~name =
    neg_lin_test ~rep_count ~count ~retries ~name ~lin_prop

  let stress_test ~count ~name = (* Note: magic constants differ for this one *)
    M.lin_test ~rep_count:25 ~count ~retries:5 ~name ~lin_prop:stress_prop
end

module Make (Spec : Spec) = Make_internal(MakeCmd(Spec))
