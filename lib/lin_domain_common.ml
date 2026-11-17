open Lin

module type Backend = sig
  type context
  val spawn_pair : context -> (unit -> 'a) -> (unit -> 'b) -> 'a * 'b
end

module Make_internal (Spec : Internal.CmdSpec [@alert "-internal"]) (B : Backend) = struct
  module M = Internal.Make(Spec) [@alert "-internal"]
  include M

  (* operate over arrays to avoid needless allocation underway *)
  let interp sut cs =
    let cs_arr = Array.of_list cs in
    let res_arr = Array.map (fun c -> Domain.cpu_relax(); Spec.run c sut) cs_arr in
    List.combine cs (Array.to_list res_arr)

  let run_parallel ~ctx (seq_pref,cmds1,cmds2) =
    let sut = Spec.init () in
    let pref_obs = interp sut seq_pref in
    let barrier = Atomic.make 2 in
    let main cmds () =
      Atomic.decr barrier;
      while Atomic.get barrier <> 0 do Domain.cpu_relax () done;
      try Ok (interp sut cmds) with exn -> Error exn
    in
    let obs1, obs2 = B.spawn_pair ctx (main cmds1) (main cmds2) in
    Spec.cleanup sut ;
    let obs1 = match obs1 with Ok v -> v | Error exn -> raise exn in
    let obs2 = match obs2 with Ok v -> v | Error exn -> raise exn in
    (pref_obs,obs1,obs2)

  (* Linearization property based on an Atomic flag *)
  let lin_prop ~ctx (seq_pref,cmds1,cmds2) =
    let pref_obs,obs1,obs2 = run_parallel ~ctx (seq_pref,cmds1,cmds2) in
    let seq_sut = Spec.init () in
    check_seq_cons pref_obs obs1 obs2 seq_sut []
      || QCheck.Test.fail_reportf "  Results incompatible with sequential execution\n\n%s"
         @@ Util.print_triple_vertical ~fig_indent:5 ~res_width:35
              (fun (c,r) -> Printf.sprintf "%s : %s" (Spec.show_cmd c) (Spec.show_res r))
              (pref_obs,obs1,obs2)

  (* "Don't crash under parallel usage" property *)
  let stress_prop ~ctx (seq_pref,cmds1,cmds2) =
    let _ = run_parallel ~ctx (seq_pref,cmds1,cmds2) in
    true

  (* Common magic constants *)
  let rep_count = 50 (* No. of repetitions of the non-deterministic property *)
  let retries = 3    (* Additional factor of repetition during shrinking *)

  let lin_test ~ctx ~count ~name =
    M.lin_test ~rep_count ~count ~retries ~name ~lin_prop:(lin_prop ~ctx)

  let neg_lin_test ~ctx ~count ~name =
    neg_lin_test ~rep_count ~count ~retries ~name ~lin_prop:(lin_prop ~ctx)

  let stress_test ~ctx ~count ~name = (* Note: magic constants differ for this one *)
    M.lin_test ~rep_count:25 ~count ~retries:5 ~name ~lin_prop:(stress_prop ~ctx)
end

module Make (Spec : Spec) (B : Backend) = Make_internal(MakeCmd(Spec))(B)
