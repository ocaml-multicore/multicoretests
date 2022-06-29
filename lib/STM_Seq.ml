open STM_Core

module Make (Spec: Spec) = struct

  open QCheck
  open STM_Core.Make(Spec)

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

  let agree_prop =
    fun cs ->
       assume (cmds_ok Spec.init_state cs);
       let sut = Spec.init_sut () in (* reset system's state *)
       let res = check_disagree Spec.init_state sut cs in
       let ()  = Spec.cleanup sut in
       match res with
       | None -> true
       | Some trace ->
           Test.fail_reportf "  Results incompatible with model\n%s"
           @@ print_seq_trace trace
  (** The agreement property: the command sequence [cs] yields the same observations
      when interpreted from the model's initial state and the [sut]'s initial state.
      Cleans up after itself by calling [Spec.cleanup] *)

  let agree_test ~count ~name =
    Test.make ~name ~count (arb_cmds Spec.init_state) agree_prop
  (** An actual agreement test (for convenience). Accepts two labeled parameters:
      [count] is the test count and [name] is the printed test name. *)

  end
