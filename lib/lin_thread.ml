open Lin

module Make_internal (Spec : Internal.CmdSpec [@alert "-internal"]) = struct
  module M = Internal.Make(Spec) [@alert "-internal"]
  include M

  let alloc_callback _src =
    Thread.yield ();
    None

  let yield_tracker =
    Gc.Memprof.{ null_tracker with alloc_minor = alloc_callback;
                                   alloc_major = alloc_callback; }

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
  let lin_prop (seq_pref, cmds1, cmds2) =
    let sut = Spec.init () in
    let obs1, obs2 = ref (Ok []), ref (Ok []) in
    (* Gc.Memprof.{start,stop} raises Failure on OCaml 5.0 and 5.1 *)
    (try ignore (Gc.Memprof.start ~sampling_rate:1e-1 ~callstack_size:0 yield_tracker) with Failure _ -> ());
    let pref_obs = interp_plain sut seq_pref in
    let wait = ref true in
    let th1 = Thread.create (fun () -> while !wait do Thread.yield () done; obs1 := try Ok (interp_thread sut cmds1) with exn -> Error exn) () in
    let th2 = Thread.create (fun () -> wait := false; obs2 := try Ok (interp_thread sut cmds2) with exn -> Error exn) () in
    Thread.join th1;
    Thread.join th2;
    (try Gc.Memprof.stop () with Failure _ -> ());
    Spec.cleanup sut;
    let obs1 = match !obs1 with Ok v -> ref v | Error exn -> raise exn in
    let obs2 = match !obs2 with Ok v -> ref v | Error exn -> raise exn in
    let seq_sut = Spec.init () in
    (* we reuse [check_seq_cons] to linearize and interpret sequentially *)
    check_seq_cons pref_obs !obs1 !obs2 seq_sut []
    || QCheck.Test.fail_reportf "  Results incompatible with sequential execution\n\n%s"
       @@ Util.print_triple_vertical ~fig_indent:5 ~res_width:35
            (fun (c,r) -> Printf.sprintf "%s : %s" (Spec.show_cmd c) (Spec.show_res r))
            (pref_obs,!obs1,!obs2)

  (* Common magic constants *)
  let rep_count = 3 (* No. of repetitions of the non-deterministic property *)
  let retries = 25  (* Additional factor of repetition during shrinking *)

  let lin_test ~count ~name =
    lin_test ~rep_count ~count ~retries ~name ~lin_prop

  let neg_lin_test ~count ~name =
    neg_lin_test ~rep_count ~count ~retries ~name ~lin_prop
end

module Make (Spec : Spec) = Make_internal(MakeCmd(Spec))
