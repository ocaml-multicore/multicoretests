open Lin
open Util

module Make_internal (Spec : Internal.CmdSpec [@alert "-internal"]) = struct
  module M = Internal.Make(Spec) [@alert "-internal"]
  include M

  (* operate over arrays to avoid needless allocation underway *)
  let interp sut cs =
    let cs_arr = Array.of_list cs in
    let res_arr = Array.map (fun c -> Spec.run c sut) cs_arr in
    List.combine cs (Array.to_list res_arr)

  let run_parallel ~pool (seq_pref,cmds1,cmds2) =
    let sut = Spec.init () in
    let pref_obs = interp sut seq_pref in
    let wait = Atomic.make true in
    let prom1 = Domain_pair.async_d1 pool (fun () -> while Atomic.get wait do Domain.cpu_relax () done; try Ok (interp sut cmds1) with exn -> Error exn) in
    let prom2 = Domain_pair.async_d2 pool (fun () -> Atomic.set wait false; try Ok (interp sut cmds2) with exn -> Error exn) in
    let obs1 = Domain_pair.await prom1 in
    let obs2 = Domain_pair.await prom2 in
    Spec.cleanup sut ;
    let obs1 = match obs1 with Ok v -> v | Error exn -> raise exn in
    let obs2 = match obs2 with Ok v -> v | Error exn -> raise exn in
    (pref_obs,obs1,obs2)

  (* Linearization property based on [Domain] and an Atomic flag *)
  let lin_prop ~pool (seq_pref,cmds1,cmds2) =
    let pref_obs,obs1,obs2 = run_parallel ~pool (seq_pref,cmds1,cmds2) in
    let seq_sut = Spec.init () in
    check_seq_cons pref_obs obs1 obs2 seq_sut []
      || QCheck.Test.fail_reportf "  Results incompatible with sequential execution\n\n%s"
         @@ Util.print_triple_vertical ~fig_indent:5 ~res_width:35
              (fun (c,r) -> Printf.sprintf "%s : %s" (Spec.show_cmd c) (Spec.show_res r))
              (pref_obs,obs1,obs2)

  (* "Don't crash under parallel usage" property *)
  let stress_prop ~pool (seq_pref,cmds1,cmds2) =
    let _ = run_parallel ~pool (seq_pref,cmds1,cmds2) in
    true

  let lin_test ~count ~name =
    let rep_count, retries = 50,3 in
    let arb_cmd_triple = arb_cmds_triple 20 12 in
    QCheck.Test.make ~count ~retries ~name
      arb_cmd_triple
      (fun triple ->
         let pool = Util.Domain_pair.init () in
         Stdlib.Fun.protect
           ~finally:(fun () -> Util.Domain_pair.takedown pool)
           (fun () -> repeat rep_count (lin_prop ~pool) triple))

  let neg_lin_test ~count ~name =
    let rep_count, retries = 50,3 in
      let arb_cmd_triple = arb_cmds_triple 20 12 in
      QCheck.Test.make_neg ~count ~retries ~name
        arb_cmd_triple
        (fun triple ->
           let pool = Util.Domain_pair.init () in
           Stdlib.Fun.protect
             ~finally:(fun () -> Util.Domain_pair.takedown pool)
             (fun () -> repeat rep_count (lin_prop ~pool) triple))

  let stress_test ~count ~name =
    let rep_count, retries = 25,5 in
      let arb_cmd_triple = arb_cmds_triple 20 12 in
      QCheck.Test.make ~count ~retries ~name
        arb_cmd_triple
        (fun triple ->
           let pool = Util.Domain_pair.init () in
           Stdlib.Fun.protect
             ~finally:(fun () -> Util.Domain_pair.takedown pool)
             (fun () -> repeat rep_count (stress_prop ~pool) triple))
end

module Make (Spec : Spec) = Make_internal(MakeCmd(Spec))
