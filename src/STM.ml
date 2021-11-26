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
  (** The initial state of the system under test. *)

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
  (*val shrink_triple : ...*)
    val arb_cmds_par : int -> int -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) arbitrary
    val agree_prop_par      : (Spec.cmd list * Spec.cmd list * Spec.cmd list) -> bool
    val agree_test_par      : count:int -> name:string -> Test.t
    val agree_test_par_nd   : count:int -> name:string -> Test.t
    val agree_test_par_comb : count:int -> name:string -> Test.t
    val agree_prop_pardomlib      : (Spec.cmd list * Spec.cmd list * Spec.cmd list) -> bool
    val agree_test_pardomlib      : count:int -> name:string -> Test.t
    val agree_test_pardomlib_nd   : count:int -> name:string -> Test.t
    val agree_test_pardomlib_comb : count:int -> name:string -> Test.t

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
    Test.make ~name:name ~count:count (arb_cmds Spec.init_state) agree_prop
  (** An actual agreement test (for convenience). Accepts two labeled parameters:
      [count] is the test count and [name] is the printed test name. *)


  (* ****************************** additions from here ****************************** *)

  let check_and_next (c,res) s =
    let b  = Spec.postcond c s res in
    let s' = Spec.next_state c s in
    b,s'

  let interp_sut_res sut cs =
    let rec interp_sut_res_acc sut cs acc = match cs with
      | [] -> List.rev acc
      | c::cs ->
         let res = Spec.run c sut in
         interp_sut_res_acc sut cs ((c,res)::acc)
    in
    interp_sut_res_acc sut cs []

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
    make ~print:print_triple ~shrink:shrink_triple gen_triple

  (* Parallel agreement property based on [Domain] *)
  let agree_prop_par =
    (fun (seq_pref,cmds1,cmds2) ->
      assume (cmds_ok Spec.init_state (seq_pref@cmds1));
      assume (cmds_ok Spec.init_state (seq_pref@cmds2));
      let sut = Spec.init_sut () in
      let pref_obs = interp_sut_res sut seq_pref in
      (*
        let doms = List.map (fu   n cs -> Domain.spawn (fun () -> interp_sut_res_acc sut cs [])) [cmds1;cmds2] in
        let res1,res2 = match List  .map Domain.join doms with [r1;r2] -> r1,r2 | _ -> failwith "Dang" in
       *)
      let dom1 = Domain.spawn (fun () -> Gc.minor(); Gc.minor(); interp_sut_res sut cmds1) in
      let dom2 = Domain.spawn (fun () -> interp_sut_res sut cmds2) in
      let obs1 = Domain.join dom1 in
      let obs2 = Domain.join dom2 in
      let res  = check_obs pref_obs obs1 obs2 Spec.init_state in
      let ()   = Spec.cleanup sut in
      res)

  (* Parallel agreement test based on [Domain] and [repeat] *)
  let agree_test_par ~count ~name =
    let rep_count = 50 in
    let seq_len,par_len = 20,15 in
    Test.make ~count ~name
      (arb_cmds_par seq_len par_len)
      (repeat rep_count agree_prop_par)

  (* Parallel agreement test based on [Domain] and [Non_det] *)
  let agree_test_par_nd ~count ~name =
    let rep_count = 200 in
    let seq_len,par_len = 20,15 in
    Non_det.Test.make ~repeat:rep_count ~count ~name
      (arb_cmds_par seq_len par_len) agree_prop_par

  (* Parallel agreement test based on [Domain] which combines [repeat] and [Non_det] *)
  let agree_test_par_comb ~count ~name =
    let rep_count = 15 in
    let seq_len,par_len = 20,15 in
    Non_det.Test.make ~repeat:15 ~count ~name
      (arb_cmds_par seq_len par_len)
      (repeat rep_count agree_prop_par) (* 15 times each, then 15 * 15 times when shrinking *)

  (* Parallel agreement property based on [Domainslib.Task] *)
  let agree_prop_pardomlib =
    (fun (seq_pref,cmds1,cmds2) ->
      let open Domainslib in
      assume (cmds_ok Spec.init_state (seq_pref@cmds1));
      assume (cmds_ok Spec.init_state (seq_pref@cmds2));
      let pool = Task.setup_pool ~num_additional_domains:2 () in
      let sut = Spec.init_sut () in
      let pref_obs = interp_sut_res sut seq_pref in
      let prm1 = Task.async pool (fun () -> (*Gc.minor();*) interp_sut_res sut cmds1) in
      let prm2 = Task.async pool (fun () -> (*Gc.minor();*) interp_sut_res sut cmds2) in
      (*Gc.minor();*)
      let obs1 = Task.await pool prm1 in
      let obs2 = Task.await pool prm2 in
      let res  = check_obs pref_obs obs1 obs2 Spec.init_state in
      let () = Spec.cleanup sut in
      let () = Task.teardown_pool pool in
      res)

  (* Parallel agreement test based on [Domainslib.Task] and [repeat] *)
  let agree_test_pardomlib ~count ~name =
    let rep_count = 50 in
    let seq_len,par_len = 20,15 in
    Test.make ~count ~name
      (arb_cmds_par seq_len par_len)
      (repeat rep_count agree_prop_pardomlib)

  (* Parallel agreement test based on [Domainslib.Task] and [Non_det] *)
  let agree_test_pardomlib_nd ~count ~name =
    let rep_count = 200 in
    let seq_len,par_len = 20,15 in
    Non_det.Test.make ~repeat:rep_count ~count ~name
      (arb_cmds_par seq_len par_len) agree_prop_pardomlib

  (* Parallel agreement test based on [Domainslib.Task] which combines [repeat] and [Non_det] *)
  let agree_test_pardomlib_comb ~count ~name =
    let rep_count = 15 in
    let seq_len,par_len = 20,15 in
    Non_det.Test.make ~repeat:15 ~count ~name
      (arb_cmds_par seq_len par_len)
      (repeat rep_count agree_prop_pardomlib) (* 15 times each, then 15 * 15 times when shrinking *)

  let agree_test_suite ~count ~name =
    let par_name = "parallel " ^ name in
    [ agree_test                ~count ~name:("sequential " ^ name);
      agree_test_par            ~count ~name:(par_name ^ " (w/repeat)");
      agree_test_par_nd         ~count ~name:(par_name ^ " (w/Non_det module)");
      agree_test_par_comb       ~count ~name:(par_name ^ " (w/repeat-Non_det comb.)");
      agree_test_pardomlib      ~count ~name:(par_name ^ " (w/Domainslib.Task and repeat)");
      agree_test_pardomlib_nd   ~count ~name:(par_name ^ " (w/Domainslib.Task and Non_det module)");
      agree_test_pardomlib_comb ~count ~name:(par_name ^ " (w/Domainslib.Task and repeat-Non_det comb.)");
    ]

end

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
