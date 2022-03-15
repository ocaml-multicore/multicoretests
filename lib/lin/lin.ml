open QCheck

module type CmdSpec = sig
  type t
  (** The type of the system under test *)

  type cmd
  (** The type of commands *)

  val show_cmd : cmd -> string
  (** [show_cmd c] returns a string representing the command [c]. *)

  val gen_cmd : cmd Gen.t
  (** A command generator. *)

  type res
  (** The command result type *)

  val show_res : res -> string
  (** [show_res r] returns a string representing the result [r]. *)

  val init : unit -> t
  (** Initialize the system under test. *)

  val cleanup : t -> unit
  (** Utility function to clean up [t] after each test instance,
      e.g., for closing sockets, files, or resetting global parameters *)

  val run : cmd -> t -> res
  (** [run c t] should interpret the command [c] over the system under test [t] (typically side-effecting). *)
end


(* Horrible copy-paste for now, please FIXME *)
let rec repeat n prop = fun input ->
  if n<0 then failwith "repeat: negative repetition count";
  if n=0
  then true
  else prop input && repeat (n-1) prop input


module Make(Spec : CmdSpec) (*: StmTest *)
  = struct

  (* Horrible copy-paste for now, please FIXME *)
  let print_triple_vertical ?(fig_indent=10) ?(res_width=20) show (seq,cmds1,cmds2) =
    let seq,cmds1,cmds2 = List.(map show seq, map show cmds1, map show cmds2) in
    let max_width ss = List.fold_left max 0 (List.map String.length ss) in
    let width = List.fold_left max 0 [max_width seq; max_width cmds1; max_width cmds2] in
    let res_width = max width res_width in
    let cmd_indent = String.make ((width-1)/2) ' ' in
    let seq_indent = String.make ((res_width + 3)/2) ' ' in
    let bar_cmd = Printf.sprintf "%-*s" res_width (cmd_indent ^ "|") in
    let center c =
      let clen = String.length c in
      if clen > width (* it's a '|'-string *)
      then c
      else Printf.sprintf "%s%s" (String.make ((width - clen)/2) ' ') c in
    let buf = Buffer.create 64 in
    let indent () = Printf.bprintf buf "%s" (String.make fig_indent ' ') in
    let print_seq_col c = Printf.bprintf buf "%s%-*s\n" seq_indent res_width c in
    let print_par_col c1 c2 = Printf.bprintf buf "%-*s  %-*s\n" res_width c1 res_width c2 in
    let print_hoz_line () =
      Printf.bprintf buf "%-*s\n" res_width (cmd_indent ^ "." ^ (String.make (res_width + 1) '-') ^ ".") in
    let rec print_par_cols cs cs' = match cs,cs' with
      | [],   []    -> ()
      | c::cs,[]    -> indent (); print_par_col (center c) ""; print_par_cols cs []
      | [],   c::cs -> indent (); print_par_col "" (center c); print_par_cols [] cs
      | l::ls,r::rs -> indent (); print_par_col (center l) (center r); print_par_cols ls rs in
    (* actual printing *)
    List.iter (fun c -> indent (); print_seq_col (center c)) ([bar_cmd] @ seq @ [bar_cmd]);
    indent (); print_hoz_line ();
    print_par_cols (bar_cmd::cmds1) (bar_cmd::cmds2);
    Buffer.contents buf

  (* operate over arrays to avoid needless allocation underway *)
  let interp sut cs =
    let cs_arr = Array.of_list cs in
    let res_arr = Array.map (fun c -> Domain.cpu_relax(); Spec.run c sut) cs_arr in
    List.combine cs (Array.to_list res_arr)

  (* use a ref to a list given by the parent thread to put the results in as a thread does not
   return a value *)
  let interp_thread res sut cs =
    let cs_arr  = Array.of_list cs in
    let res_arr = Array.map (fun c -> Thread.yield (); Spec.run c sut) cs_arr in
    res := List.combine cs (Array.to_list res_arr)
    
  let rec gen_cmds fuel =
    Gen.(if fuel = 0
         then return []
         else
  	  Spec.gen_cmd >>= fun c ->
	   gen_cmds (fuel-1) >>= fun cs ->
             return (c::cs))
  (** A fueled command list generator.
      Accepts a state parameter to enable state-dependent [cmd] generation. *)

  let gen_cmds_size size_gen = Gen.sized_size size_gen gen_cmds

  let shrink_triple =
    let open Iter in
      (fun (seq,p1,p2) ->
        (map (fun seq' -> (seq',p1,p2)) (Shrink.list (*~shrink:shrink_cmd*) seq))
        <+>
        (match p1 with [] -> Iter.empty | c1::c1s -> Iter.return (seq@[c1],c1s,p2))
        <+>
        (match p2 with [] -> Iter.empty | c2::c2s -> Iter.return (seq@[c2],p1,c2s))
        <+>
        (map (fun p1' -> (seq,p1',p2)) (Shrink.list (*~shrink:shrink_cmd*) p1))
        <+>
        (map (fun p2' -> (seq,p1,p2')) (Shrink.list (*~shrink:shrink_cmd*) p2)))

  let arb_cmds_par seq_len par_len =
    let seq_pref_gen = gen_cmds_size (Gen.int_bound seq_len) in
    let par_gen = gen_cmds_size (Gen.int_bound par_len) in
    let gen_triple = Gen.(triple seq_pref_gen par_gen par_gen) in
    make ~print:(print_triple_vertical Spec.show_cmd) ~shrink:shrink_triple gen_triple

  let rec check_seq_cons pref cs1 cs2 seq_sut seq_trace = match pref with
    | (c,res)::pref' ->
       res = Spec.run c seq_sut
       && check_seq_cons pref' cs1 cs2 seq_sut (c::seq_trace)
    | [] -> match cs1,cs2 with
            | [],[] -> true
            | [],(c2,res2)::cs2' ->
               res2 = Spec.run c2 seq_sut
               && check_seq_cons pref cs1 cs2' seq_sut (c2::seq_trace)
            | (c1,res1)::cs1',[] ->
               res1 = Spec.run c1 seq_sut
               && check_seq_cons pref cs1' cs2 seq_sut (c1::seq_trace)
            | (c1,res1)::cs1',(c2,res2)::cs2' ->
               (res1 = Spec.run c1 seq_sut
                && check_seq_cons pref cs1' cs2 seq_sut (c1::seq_trace))
               ||
                 (* rerun to get seq_sut to same cmd branching point *)
                 (Spec.cleanup seq_sut;
                  let seq_sut' = Spec.init () in
                  let _ = interp seq_sut' (List.rev seq_trace) in
                  let b =
                    res2 = Spec.run c2 seq_sut'
                    && check_seq_cons pref cs1 cs2' seq_sut' (c2::seq_trace)
                  in
                  Spec.cleanup seq_sut'; b)

  (* Linearizability property based on [Domain] and an Atomic flag *)
  let lin_prop_domain =
    (fun (seq_pref,cmds1,cmds2) ->
      let sut = Spec.init () in
      let pref_obs = interp sut seq_pref in
      let wait = Atomic.make true in
      let dom1 = Domain.spawn (fun () -> while Atomic.get wait do Domain.cpu_relax() done; interp sut cmds1) in
      let dom2 = Domain.spawn (fun () -> Atomic.set wait false; interp sut cmds2) in
      let obs1 = Domain.join dom1 in
      let obs2 = Domain.join dom2 in
      let ()   = Spec.cleanup sut in
      let seq_sut = Spec.init () in
      let b = check_seq_cons pref_obs obs1 obs2 seq_sut [] in
      Spec.cleanup seq_sut;
      b
      || Test.fail_reportf "  Results incompatible with sequential execution\n\n%s"
         @@ print_triple_vertical ~fig_indent:5 ~res_width:35
              (fun (c,r) -> Printf.sprintf "%s : %s" (Spec.show_cmd c) (Spec.show_res r))
              (pref_obs,obs1,obs2))

  (* Linearizability property based on [Thread] *)
  let lin_prop_thread =
    (fun (seq_pref, cmds1, cmds2) ->
      let sut = Spec.init () in
      let pref_obs, obs1, obs2 = ref [], ref [], ref [] in
      interp_thread pref_obs sut seq_pref;
      let th1 = Thread.create (Thread.yield (); interp_thread obs1 sut) cmds1 in
      let th2 = Thread.create (interp_thread obs2 sut) cmds2 in
      Thread.join th1; Thread.join th2; Spec.cleanup sut;
      let seq_sut = Spec.init () in
      (* we should be able to reuse [check_seq_cons] *)
      let b = check_seq_cons !pref_obs !obs1 !obs2 seq_sut [] in
      Spec.cleanup seq_sut;
      b
      || Test.fail_reportf "  Results incompatible with sequential execution\n\n%s"
         @@ print_triple_vertical ~fig_indent:5 ~res_width:35
              (fun (c,r) -> Printf.sprintf "%s : %s" (Spec.show_cmd c) (Spec.show_res r))
              (!pref_obs,!obs1,!obs2))
  
  (* Linearizability test based on [Domain] *)
  let lin_test ~count ~name (lib : [ `Domain | `Thread ]) =
    let rep_count = 50 in
    let seq_len,par_len = 20,15 in
    let lin_prop = match lib with `Domain -> lin_prop_domain | ` Thread -> lin_prop_thread in
    Test.make ~count ~retries:3 ~name:("Linearizable " ^ name)
      (arb_cmds_par seq_len par_len) (repeat rep_count lin_prop)
end
