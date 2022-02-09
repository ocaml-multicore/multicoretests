open QCheck

(** A revised state machine framework with parallel testing.
    This version does not come with built-in GC commands. *)

(** The specification of a state machine. *)
module type StmSpec =
sig
  type cmd
  (** The type of commands *)

  type state
  (** The type of the model's state *)

  type sut
  (** The type of the system under test *)

  val arb_cmd : state -> cmd arbitrary
  (** A command generator. Accepts a state parameter to enable state-dependent [cmd] generation. *)

  val init_state : state
  (** The model's initial state. *)

  val next_state : cmd -> state -> state
  (** Move the internal state machine to the next state. *)


  val init_sut : unit -> sut
  (** Initialize the system under test. *)

  val cleanup : sut -> unit
  (** Utility function to clean up the [sut] after each test instance,
      e.g., for closing sockets, files, or resetting global parameters*)

  (*val run_cmd : cmd -> state -> sut -> bool*)
  (** [run_cmd c s i] should interpret the command [c] over the system under test (typically side-effecting).
      [s] is in this case the model's state prior to command execution.
      The returned Boolean value should indicate whether the interpretation went well
      and in case [c] returns a value: whether the returned value agrees with the model's result. *)

  val precond : cmd -> state -> bool
  (** [precond c s] expresses preconditions for command [c].
      This is useful, e.g., to prevent the shrinker from breaking invariants when minimizing
      counterexamples. *)

  (* ************************ additions from here ************************ *)
  val show_cmd : cmd -> string
  (** [show_cmd c] returns a string representing the command [c]. *)

  type res
  (** The command result type *)

  val show_res : res -> string
  (** [show_res r] returns a string representing the result [r]. *)

  val run : cmd -> sut -> res
  (** [run c i] should interpret the command [c] over the system under test (typically side-effecting). *)

  val postcond : cmd -> state -> res -> bool
  (** [postcond c s res] checks whether [res] arising from interpreting the
      command [c] over the system under test with [run] agrees with the
      model's result.
      Note: [s] is in this case the model's state prior to command execution. *)
end

let rec repeat n prop = fun input ->
  if n<0 then failwith "repeat: negative repetition count";
  if n=0
  then true
  else prop input && repeat (n-1) prop input


(** Derives a test framework from a state machine specification. *)
module Make(Spec : StmSpec) (*: StmTest *)
  : sig
    val cmds_ok : Spec.state -> Spec.cmd list -> bool
    val gen_cmds : Spec.state -> int -> Spec.cmd list Gen.t
    val arb_cmds : Spec.state -> Spec.cmd list arbitrary
    val consistency_test : count:int -> name:string -> Test.t
    val interp_agree : Spec.state -> Spec.sut -> Spec.cmd list -> bool
    val agree_prop : Spec.cmd list -> bool
    val agree_test : count:int -> name:string -> Test.t

    (* ****************************** additions from here ****************************** *)

  (*val check_and_next : (Spec.cmd * Spec.res) -> Spec.state -> bool * Spec.state*)
    val interp_sut_res : Spec.sut -> Spec.cmd list -> (Spec.cmd * Spec.res) list
    val check_obs : (Spec.cmd * Spec.res) list -> (Spec.cmd * Spec.res) list -> (Spec.cmd * Spec.res) list -> Spec.state -> bool
    val gen_cmds_size : Spec.state -> int Gen.t -> Spec.cmd list Gen.t
  (*val print_triple : ...*)
    val print_triple_vertical : ?fig_indent:int -> ?res_width:int -> ('a -> string) -> ('a list * 'a list * 'a list) -> string
  (*val shrink_triple : ...*)
    val arb_cmds_par : int -> int -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) arbitrary
    val agree_prop_par         : (Spec.cmd list * Spec.cmd list * Spec.cmd list) -> bool
    val agree_test_par         : count:int -> name:string -> Test.t
    val agree_test_par_retries : count:int -> name:string -> Test.t
    val agree_test_par_comb    : count:int -> name:string -> Test.t
    val agree_prop_pardomlib         : (Spec.cmd list * Spec.cmd list * Spec.cmd list) -> bool
    val agree_test_pardomlib         : count:int -> name:string -> Test.t
    val agree_test_pardomlib_retries : count:int -> name:string -> Test.t
    val agree_test_pardomlib_comb    : count:int -> name:string -> Test.t

    val agree_test_suite    : count:int -> name:string -> Test.t list
end
=
struct
  (** {3 The resulting test framework derived from a state machine specification} *)

  let rec gen_cmds s fuel =
    Gen.(if fuel = 0
         then return []
         else
  	  (Spec.arb_cmd s).gen >>= fun c ->
	   (gen_cmds (Spec.next_state c s) (fuel-1)) >>= fun cs ->
             return (c::cs))
  (** A fueled command list generator.
      Accepts a state parameter to enable state-dependent [cmd] generation. *)

  let rec cmds_ok s cs = match cs with
    | [] -> true
    | c::cs ->
      Spec.precond c s &&
	let s' = Spec.next_state c s in
	cmds_ok s' cs
  (** A precondition checker (stops early, thanks to short-circuit Boolean evaluation).
      Accepts the initial state and the command sequence as parameters.  *)

  let arb_cmds s =
    let cmds_gen = Gen.sized (gen_cmds s) in
    let shrinker = match (Spec.arb_cmd s).shrink with
                    | None   -> Shrink.list ~shrink:Shrink.nil (* no elem. shrinker provided *)
                    | Some s -> Shrink.list ~shrink:s in
    let ac = QCheck.make ~shrink:(Shrink.filter (cmds_ok Spec.init_state) shrinker) cmds_gen in
    (match (Spec.arb_cmd s).print with
     | None   -> ac
     | Some p -> set_print (Print.list p) ac)
  (** A generator of command sequences. Accepts the initial state as parameter. *)

  let consistency_test ~count ~name =
    Test.make ~name:name ~count:count (arb_cmds Spec.init_state) (cmds_ok Spec.init_state)
  (** A consistency test that generates a number of [cmd] sequences and
      checks that all contained [cmd]s satisfy the precondition [precond].
      Accepts two labeled parameters:
      [count] is the test count and [name] is the printed test name. *)

  let rec interp_agree s sut cs = match cs with
    | [] -> true
    | c::cs ->
      let res = Spec.run c sut in
      let b   = Spec.postcond c s res in
      let s'  = Spec.next_state c s in
      b && interp_agree s' sut cs
  (** Checks agreement between the model and the system under test
      (stops early, thanks to short-circuit Boolean evaluation). *)

  let agree_prop =
    (fun cs ->
       assume (cmds_ok Spec.init_state cs);
       let sut = Spec.init_sut () in (* reset system's state *)
       let res = interp_agree Spec.init_state sut cs in
       let ()  = Spec.cleanup sut in
       res)
  (** The agreement property: the command sequence [cs] yields the same observations
      when interpreted from the model's initial state and the [sut]'s initial state.
      Cleans up after itself by calling [Spec.cleanup] *)

  let agree_test ~count ~name =
    Test.make ~name:("sequential " ^ name) ~count (arb_cmds Spec.init_state) agree_prop
  (** An actual agreement test (for convenience). Accepts two labeled parameters:
      [count] is the test count and [name] is the printed test name. *)


  (* ****************************** additions from here ****************************** *)

  let check_and_next (c,res) s =
    let b  = Spec.postcond c s res in
    let s' = Spec.next_state c s in
    b,s'

  (* operate over arrays to avoid needless allocation underway *)
  let interp_sut_res sut cs =
    let cs_arr = Array.of_list cs in
    let res_arr = Array.map (fun c -> Domain.cpu_relax(); Spec.run c sut) cs_arr in
    List.combine cs (Array.to_list res_arr)

  let rec check_obs pref cs1 cs2 s = match pref with
    | p::pref' ->
       let b,s' = check_and_next p s in
       b && check_obs pref' cs1 cs2 s'
    | [] ->
       match cs1,cs2 with
       | [],[] -> true
       | [],p2::cs2' ->
          let b,s' = check_and_next p2 s in
          b && check_obs pref cs1 cs2' s'
       | p1::cs1',[] ->
          let b,s' = check_and_next p1 s in
          b && check_obs pref cs1' cs2 s'
       | p1::cs1',p2::cs2' ->
          (let b1,s' = check_and_next p1 s in
           b1 && check_obs pref cs1' cs2 s')
          ||
          (let b2,s' = check_and_next p2 s in
           b2 && check_obs pref cs1 cs2' s')

  let gen_cmds_size s size_gen = Gen.sized_size size_gen (gen_cmds s)
(*
  let print_triple (seq,par1,par2) =
    let header1, header2 = "Seq.prefix:", "Parallel procs.:" in
    let pr_cmds = Print.list Spec.show_cmd in
    let seq_str = pr_cmds seq in
    let seq_len = max (String.length header1) (String.length seq_str) in
    let buf = Buffer.create 64 in
    begin
      Printf.bprintf buf " %-*s  %s\n\n" seq_len header1 header2;
      Printf.bprintf buf " %s  %s\n" (String.make seq_len ' ') (pr_cmds par1);
      Printf.bprintf buf " %*s\n" seq_len seq_str;
      Printf.bprintf buf " %s  %s\n" (String.make seq_len ' ') (pr_cmds par2);
      Buffer.contents buf
    end
 *)
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

  let shrink_triple =
    let open Iter in
    let shrink_cmd = Option.value Spec.(arb_cmd init_state).shrink ~default:Shrink.nil in
    Shrink.filter
      (fun (seq,p1,p2) -> cmds_ok Spec.init_state (seq@p1) && cmds_ok Spec.init_state (seq@p2))
      (fun (seq,p1,p2) ->
        (map (fun seq' -> (seq',p1,p2)) (Shrink.list ~shrink:shrink_cmd seq))
        <+>
        (match p1 with [] -> Iter.empty | c1::c1s -> Iter.return (seq@[c1],c1s,p2))
        <+>
        (match p2 with [] -> Iter.empty | c2::c2s -> Iter.return (seq@[c2],p1,c2s))
        <+>
        (map (fun p1' -> (seq,p1',p2)) (Shrink.list ~shrink:shrink_cmd p1))
        <+>
        (map (fun p2' -> (seq,p1,p2')) (Shrink.list ~shrink:shrink_cmd p2)))

  let arb_cmds_par seq_len par_len =
    let seq_pref_gen = gen_cmds_size Spec.init_state (Gen.int_bound seq_len) in
    let gen_triple =
      Gen.(seq_pref_gen >>= fun seq_pref ->
           let spawn_state = List.fold_left (fun st c -> Spec.next_state c st) Spec.init_state seq_pref in
           let par = gen_cmds_size spawn_state (Gen.int_bound par_len) in
           map2 (fun par1 par2 -> (seq_pref,par1,par2)) par par) in
    make ~print:(print_triple_vertical Spec.show_cmd) ~shrink:shrink_triple gen_triple

  (* Parallel agreement property based on [Domain] *)
  let agree_prop_par =
    (fun (seq_pref,cmds1,cmds2) ->
      assume (cmds_ok Spec.init_state (seq_pref@cmds1));
      assume (cmds_ok Spec.init_state (seq_pref@cmds2));
      let sut = Spec.init_sut () in
      let pref_obs = interp_sut_res sut seq_pref in
      let wait = Atomic.make true in
      let dom1 = Domain.spawn (fun () -> while Atomic.get wait do Domain.cpu_relax() done; interp_sut_res sut cmds1) in
      let dom2 = Domain.spawn (fun () -> Atomic.set wait false; interp_sut_res sut cmds2) in
      let obs1 = Domain.join dom1 in
      let obs2 = Domain.join dom2 in
      let ()   = Spec.cleanup sut in
      check_obs pref_obs obs1 obs2 Spec.init_state
      || Test.fail_reportf "  Results incompatible with linearized model\n\n%s"
         @@ print_triple_vertical ~fig_indent:5 ~res_width:35
              (fun (c,r) -> Printf.sprintf "%s : %s" (Spec.show_cmd c) (Spec.show_res r))
              (pref_obs,obs1,obs2))

  (* Parallel agreement test based on [Domain] and [repeat] *)
  let agree_test_par ~count ~name =
    let rep_count = 50 in
    let seq_len,par_len = 20,15 in
    Test.make ~count ~name:("parallel " ^ name ^ " (w/repeat)")
      (arb_cmds_par seq_len par_len)
      (repeat rep_count agree_prop_par)

  (* Parallel agreement test based on [Domain] and [~retries] *)
  let agree_test_par_retries ~count ~name =
    let rep_count = 200 in
    let seq_len,par_len = 20,15 in
    Test.make ~retries:rep_count ~count ~name:("parallel " ^ name ^ " (w/shrink retries)")
      (arb_cmds_par seq_len par_len) agree_prop_par

  (* Parallel agreement test based on [Domain] which combines [repeat] and [~retries] *)
  let agree_test_par_comb ~count ~name =
    let rep_count = 15 in
    let seq_len,par_len = 20,15 in
    Test.make ~retries:15 ~count ~name:("parallel " ^ name ^ " (w/repeat-retries comb.)")
      (arb_cmds_par seq_len par_len)
      (repeat rep_count agree_prop_par) (* 15 times each, then 15 * 15 times when shrinking *)

  (* Parallel agreement property based on [Domainslib.Task] *)
  let agree_prop_pardomlib =
    (fun (seq_pref,cmds1,cmds2) ->
      let open Domainslib in
      assume (cmds_ok Spec.init_state (seq_pref@cmds1));
      assume (cmds_ok Spec.init_state (seq_pref@cmds2));
      let pool = Task.setup_pool ~num_additional_domains:2 () in
      let pref_obs,obs1,obs2 =
        Task.run pool (fun () ->
            let sut = Spec.init_sut () in
            let pref_obs = interp_sut_res sut seq_pref in
            let prm1 = Task.async pool (fun () -> (*Gc.minor();*) interp_sut_res sut cmds1) in
            let prm2 = Task.async pool (fun () -> (*Gc.minor();*) interp_sut_res sut cmds2) in
            (*Gc.minor();*)
            let obs1 = Task.await pool prm1 in
            let obs2 = Task.await pool prm2 in
            let () = Spec.cleanup sut in
            pref_obs,obs1,obs2) in
      let () = Task.teardown_pool pool in
      check_obs pref_obs obs1 obs2 Spec.init_state
      || Test.fail_reportf "  Results incompatible with linearized model\n\n%s"
         @@ print_triple_vertical ~fig_indent:5 ~res_width:30
              (fun (c,r) -> Printf.sprintf "%s : %s" (Spec.show_cmd c) (Spec.show_res r))
              (pref_obs,obs1,obs2))

  (* Parallel agreement test based on [Domainslib.Task] and [repeat] *)
  let agree_test_pardomlib ~count ~name =
    let rep_count = 50 in
    let seq_len,par_len = 20,15 in
    Test.make ~count ~name:("parallel " ^ name ^ " (w/Domainslib.Task and repeat)")
      (arb_cmds_par seq_len par_len)
      (repeat rep_count agree_prop_pardomlib)

  (* Parallel agreement test based on [Domainslib.Task] and [~retries] *)
  let agree_test_pardomlib_retries ~count ~name =
    let rep_count = 200 in
    let seq_len,par_len = 20,15 in
    Test.make ~retries:rep_count ~count ~name:("parallel " ^ name ^ " (w/Domainslib.Task and shrink retries)")
      (arb_cmds_par seq_len par_len) agree_prop_pardomlib

  (* Parallel agreement test based on [Domainslib.Task] which combines [repeat] and [~retries] *)
  let agree_test_pardomlib_comb ~count ~name =
    let rep_count = 15 in
    let seq_len,par_len = 20,15 in
    Test.make ~retries:15 ~count ~name:("parallel " ^ name ^ " (w/Domainslib.Task and repeat-retries comb.)")
      (arb_cmds_par seq_len par_len)
      (repeat rep_count agree_prop_pardomlib) (* 15 times each, then 15 * 15 times when shrinking *)

  let agree_test_suite ~count ~name =
    [ agree_test                   ~count ~name;
      agree_test_par               ~count ~name;
      agree_test_par_retries       ~count ~name;
      agree_test_par_comb          ~count ~name;
      agree_test_pardomlib         ~count ~name;
      agree_test_pardomlib_retries ~count ~name;
      agree_test_pardomlib_comb    ~count ~name;
    ]

end

(** ********************************************************************** *)

module AddGC(Spec : StmSpec) : StmSpec
=
struct
  type cmd =
    | GC_minor
    | UserCmd of Spec.cmd

  type state = Spec.state
  type sut   = Spec.sut

  let init_state  = Spec.init_state
  let init_sut () = Spec.init_sut ()
  let cleanup sut = Spec.cleanup sut

  let show_cmd c = match c with
    | GC_minor -> "<GC.minor>"
    | UserCmd c -> Spec.show_cmd c

  let gen_cmd s =
    (Gen.frequency
       [(1,Gen.return GC_minor);
        (5,Gen.map (fun c -> UserCmd c) (Spec.arb_cmd s).gen)])

  let shrink_cmd s c = match c with
    | GC_minor  -> Iter.empty
    | UserCmd c ->
       match (Spec.arb_cmd s).shrink with
       | None     -> Iter.empty (* no shrinker provided *)
       | Some shk -> Iter.map (fun c' -> UserCmd c') (shk c)

  let arb_cmd s = make ~print:show_cmd ~shrink:(shrink_cmd s) (gen_cmd s)

  let next_state c s = match c with
    | GC_minor  -> s
    | UserCmd c -> Spec.next_state c s

  let precond c s = match c with
    | GC_minor  -> true
    | UserCmd c -> Spec.precond c s

  type res =
    | GCRes
    | UserRes of Spec.res

  let show_res c = match c with
    | GCRes     -> "<RGC.minor>"
    | UserRes r -> Spec.show_res r

  let run c s = match c with
    | GC_minor  -> (Gc.minor (); GCRes)
    | UserCmd c -> UserRes (Spec.run c s)

  let postcond c s r = match c,r with
    | GC_minor,  GCRes     -> true
    | UserCmd c, UserRes r -> Spec.postcond c s r
    | _,_ -> false
end
