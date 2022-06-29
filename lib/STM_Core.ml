open QCheck
include Util

(* re-export Spec *)
include Spec

(** A revised state machine framework with parallel testing.
    This version does not come with built-in GC commands. *)

(** Derives a test framework from a state machine specification. *)
module Make(Spec : Spec)
  : sig
    val cmds_ok : Spec.state -> Spec.cmd list -> bool
    (* val gen_cmds : Spec.state -> int -> Spec.cmd list Gen.t *)
    val arb_cmds : Spec.state -> Spec.cmd list arbitrary
    (* val consistency_test : count:int -> name:string -> Test.t *)
    val interp_agree : Spec.state -> Spec.sut -> Spec.cmd list -> bool
    val print_seq_trace : (Spec.cmd * res) list -> string
    val check_disagree : Spec.state -> Spec.sut -> Spec.cmd list -> (Spec.cmd * res) list option

  (*val check_and_next : (Spec.cmd * res) -> Spec.state -> bool * Spec.state*)
    val check_obs : (Spec.cmd * res) list -> (Spec.cmd * res) list -> (Spec.cmd * res) list -> Spec.state -> bool
    val gen_cmds_size : (Spec.state -> Spec.cmd arbitrary) -> Spec.state -> int Gen.t -> Spec.cmd list Gen.t
  (*val shrink_triple : ...*)
    val arb_cmds_par : int -> int -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) arbitrary
    val all_interleavings_ok : Spec.cmd list -> Spec.cmd list -> Spec.cmd list -> Spec.state -> bool
    val shrink_triple : (Spec.state -> Spec.cmd arbitrary) -> (Spec.state -> Spec.cmd arbitrary) -> (Spec.state -> Spec.cmd arbitrary) -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) Shrink.t
    val arb_triple: int -> int -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary
end
=
struct
  (** {3 The resulting test framework derived from a state machine specification} *)

  let rec gen_cmds arb s fuel =
    Gen.(if fuel = 0
         then return []
         else
  	  (arb s).gen >>= fun c ->
	   (gen_cmds arb (Spec.next_state c s) (fuel-1)) >>= fun cs ->
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
    let cmds_gen = Gen.sized (gen_cmds Spec.arb_cmd s) in
    let shrinker = match (Spec.arb_cmd s).shrink with
                    | None   -> Shrink.list ~shrink:Shrink.nil (* no elem. shrinker provided *)
                    | Some s -> Shrink.list ~shrink:s in
    let ac = QCheck.make ~shrink:(Shrink.filter (cmds_ok Spec.init_state) shrinker) cmds_gen in
    (match (Spec.arb_cmd s).print with
     | None   -> ac
     | Some p -> set_print (Print.list p) ac)
  (** A generator of command sequences. Accepts the initial state as parameter. *)

  let rec interp_agree s sut cs = match cs with
    | [] -> true
    | c::cs ->
      let res = Spec.run c sut in
      let b   = Spec.postcond c s res in
      let s'  = Spec.next_state c s in
      b && interp_agree s' sut cs
  (** Checks agreement between the model and the system under test
      (stops early, thanks to short-circuit Boolean evaluation). *)

  let rec check_disagree s sut cs = match cs with
    | [] -> None
    | c::cs ->
      let res = Spec.run c sut in
      let b   = Spec.postcond c s res in
      let s'  = Spec.next_state c s in
      if b
      then
        match check_disagree s' sut cs with
        | None -> None
        | Some rest -> Some ((c,res)::rest)
      else Some [c,res]

  let print_seq_trace trace =
    List.fold_left
      (fun acc (c,r) -> Printf.sprintf "%s\n   %s : %s" acc (Spec.show_cmd c) (show_res r))
      "" trace

  (* ****************************** additions from here ****************************** *)

  let check_and_next (c,res) s =
    let b  = Spec.postcond c s res in
    let s' = Spec.next_state c s in
    b,s'

  (* checks that all interleavings of a cmd triple satisfies all preconditions *)
  let rec all_interleavings_ok pref cs1 cs2 s =
    match pref with
    | c::pref' ->
        Spec.precond c s &&
        let s' = Spec.next_state c s in
        all_interleavings_ok pref' cs1 cs2 s'
    | [] ->
        match cs1,cs2 with
        | [],[] -> true
        | [],c2::cs2' ->
            Spec.precond c2 s &&
            let s' = Spec.next_state c2 s in
            all_interleavings_ok pref cs1 cs2' s'
        | c1::cs1',[] ->
            Spec.precond c1 s &&
            let s' = Spec.next_state c1 s in
            all_interleavings_ok pref cs1' cs2 s'
        | c1::cs1',c2::cs2' ->
            (Spec.precond c1 s &&
             let s' = Spec.next_state c1 s in
             all_interleavings_ok pref cs1' cs2 s')
            &&
            (Spec.precond c2 s &&
             let s' = Spec.next_state c2 s in
             all_interleavings_ok pref cs1 cs2' s')

  let rec check_obs pref cs1 cs2 s =
    match pref with
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

  let gen_cmds_size gen s size_gen = Gen.sized_size size_gen (gen_cmds gen s)

  (* Shrinks a single cmd, starting in the given state *)
  let shrink_cmd arb cmd state =
    Option.value (arb state).shrink ~default:Shrink.nil @@ cmd

  (* Shrinks cmd lists, starting in the given state *)
  let rec shrink_cmd_list arb cs state = match cs with
    | [] -> Iter.empty
    | c::cs ->
        if Spec.precond c state
        then
          Iter.(
            map (fun c -> c::cs) (shrink_cmd arb c state)
            <+>
            map (fun cs -> c::cs) (shrink_cmd_list arb cs Spec.(next_state c state))
          )
        else Iter.empty

  (* Shrinks cmd elements in triples *)
  let shrink_triple_elems arb0 arb1 arb2 (seq,p1,p2) =
    let shrink_prefix cs state =
      Iter.map (fun cs -> (cs,p1,p2)) (shrink_cmd_list arb0 cs state)
    in
    let rec shrink_par_suffix cs state = match cs with
      | [] ->
          (* try only one option: p1s or p2s first - both valid interleavings *)
          Iter.(map (fun p1 -> (seq,p1,p2)) (shrink_cmd_list arb1 p1 state)
                <+>
                map (fun p2 -> (seq,p1,p2)) (shrink_cmd_list arb2 p2 state))
      | c::cs ->
          (* walk seq prefix (again) to advance state *)
          if Spec.precond c state
          then shrink_par_suffix cs Spec.(next_state c state)
          else Iter.empty
    in
    match Spec.(arb_cmd init_state).shrink with
    | None -> Iter.empty (* stop early if no cmd shrinker is available *)
    | Some _ ->
        Iter.(shrink_prefix seq Spec.init_state
              <+>
              shrink_par_suffix seq Spec.init_state)

  (* General shrinker of cmd triples *)
  let shrink_triple arb0 arb1 arb2 =
    let open Iter in
    Shrink.filter
      (fun (seq,p1,p2) -> all_interleavings_ok seq p1 p2 Spec.init_state)
      (fun ((seq,p1,p2) as triple) ->
        (* Shrinking heuristic:
￼          First reduce the cmd list sizes as much as possible, since the interleaving
￼          is most costly over long cmd lists. *)
        (map (fun seq' -> (seq',p1,p2)) (Shrink.list_spine seq))
        <+>
        (match p1 with [] -> Iter.empty | c1::c1s -> Iter.return (seq@[c1],c1s,p2))
        <+>
        (match p2 with [] -> Iter.empty | c2::c2s -> Iter.return (seq@[c2],p1,c2s))
        <+>
        (map (fun p1' -> (seq,p1',p2)) (Shrink.list_spine p1))
        <+>
        (map (fun p2' -> (seq,p1,p2')) (Shrink.list_spine p2))
        <+>
        (* Secondly reduce the cmd data of individual list elements *)
        (shrink_triple_elems arb0 arb1 arb2 triple))

 let arb_triple seq_len par_len arb0 arb1 arb2 =
    let seq_pref_gen = gen_cmds_size arb0 Spec.init_state (Gen.int_bound seq_len) in
    let shrink_triple = shrink_triple arb0 arb1 arb2 in
    let gen_triple =
      Gen.(seq_pref_gen >>= fun seq_pref ->
           int_range 2 (2*par_len) >>= fun dbl_plen ->
           let spawn_state = List.fold_left (fun st c -> Spec.next_state c st) Spec.init_state seq_pref in
           let par_len1 = dbl_plen/2 in
           let par_gen1 = gen_cmds_size arb1 spawn_state (return par_len1) in
           let par_gen2 = gen_cmds_size arb2 spawn_state (return (dbl_plen - par_len1)) in
           triple (return seq_pref) par_gen1 par_gen2) in
    make ~print:(print_triple_vertical Spec.show_cmd) ~shrink:shrink_triple gen_triple

  let arb_cmds_par seq_len par_len = arb_triple seq_len par_len Spec.arb_cmd Spec.arb_cmd Spec.arb_cmd
end

(** ********************************************************************** *)

module AddGC(Spec : Spec) : Spec
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

  let run c s = match c with
    | GC_minor  -> (Gc.minor (); Res (unit, ()))
    | UserCmd c -> Spec.run c s

  let postcond c s r = match c,r with
    | GC_minor,  Res ((Unit,_),_) -> true
    | UserCmd c, r -> Spec.postcond c s r
    | _,_ -> false
end
