(* parallel stress tests of the GC with explicit Gc invocations *)

module Spec =
struct
  type cmd =
    | Set_minor_heap_size_2048
    | Compact
    | PreAllocList of int * unit list
    | RevList of int

  let show_cmd x = match x with
    | Set_minor_heap_size_2048 -> "Set minor_heap_size 2048"
    | Compact     -> "Compact"
    | PreAllocList (i,_l) -> "PreAllocList " ^ (string_of_int i)
    | RevList i   -> "RevList " ^ (string_of_int i)

  let init_state = ()

  let array_length = 4

  let gc_cmds =
    let open QCheck in
    let list_gen = Gen.map (fun l -> List.init l (fun _ -> ())) Gen.nat in
    let index_gen = Gen.int_bound (array_length-1) in
    Gen.([
        5, map2 (fun index list -> PreAllocList (index,list)) index_gen list_gen;
        5, map (fun index -> RevList index) index_gen;
        1, return Set_minor_heap_size_2048;
        1, return Compact;
      ])

  let arb_cmd _s = QCheck.(make ~print:show_cmd (Gen.frequency gc_cmds))

  let next_state _n _s = ()

  let init_sut () = Array.make array_length []

  let orig_control = Gc.get ()

  let cleanup sut =
    begin
      for i=0 to array_length-1 do
        sut.(i) <- [];
      done;
      Gc.set orig_control;
      Gc.major ()
    end

  let run c sut = match c with
    | Set_minor_heap_size_2048 -> Gc.set { orig_control with minor_heap_size = 2048 }
    | Compact     -> Gc.compact ()
    | PreAllocList (i,l) -> sut.(i) <- l (*alloc list in parent domain in test-input*)
    | RevList i -> sut.(i) <- List.rev sut.(i) (*alloc list at test runtime*)
end

let rec gen_cmds arb s fuel =
  QCheck.Gen.(if fuel = 0
       then return []
       else
         (arb s).QCheck.gen >>= fun c ->
         let s' = try Spec.next_state c s with _ -> s in
         (gen_cmds arb s' (fuel-1)) >>= fun cs ->
         return (c::cs))

let gen_cmds_size gen s size_gen = QCheck.Gen.sized_size size_gen (gen_cmds gen s)

let arb_triple seq_len par_len arb0 arb1 arb2 =
  let seq_pref_gen = gen_cmds_size arb0 Spec.init_state (QCheck.Gen.int_bound seq_len) in
(*let shrink_triple = shrink_triple arb0 arb1 arb2 in*)
  let gen_triple =
    QCheck.Gen.(seq_pref_gen >>= fun seq_pref ->
         int_range 2 (2*par_len) >>= fun dbl_plen ->
         let spawn_state = List.fold_left (fun st c -> try Spec.next_state c st with _ -> st) Spec.init_state seq_pref in
         let par_len1 = dbl_plen/2 in
         let par_gen1 = gen_cmds_size arb1 spawn_state (return par_len1) in
         let par_gen2 = gen_cmds_size arb2 spawn_state (return (dbl_plen - par_len1)) in
         triple (return seq_pref) par_gen1 par_gen2) in
  QCheck.make ~print:(Util.print_triple_vertical Spec.show_cmd) (*~shrink:shrink_triple*) gen_triple

let arb_cmds_triple seq_len par_len = arb_triple seq_len par_len Spec.arb_cmd Spec.arb_cmd Spec.arb_cmd

let arb_cmds_triple = arb_cmds_triple

let interp_sut_res sut cs =
  let cs_arr = Array.of_list cs in
  let res_arr = Array.map (fun c -> Domain.cpu_relax(); Spec.run c sut) cs_arr in
  List.combine cs (Array.to_list res_arr)

let stress_prop_par (seq_pref,cmds1,cmds2) =
  let sut = Spec.init_sut () in
  let _pref_obs = interp_sut_res sut seq_pref in
  let barrier = Atomic.make 2 in
  let main cmds () =
    Atomic.decr barrier;
    while Atomic.get barrier <> 0 do Domain.cpu_relax() done;
    try Ok (interp_sut_res sut cmds) with exn -> Error exn
  in
  let dom1 = Domain.spawn (main cmds1) in
  let dom2 = Domain.spawn (main cmds2) in
  let obs1 = Domain.join dom1 in
  let obs2 = Domain.join dom2 in
  let ()   = Spec.cleanup sut in
  let _obs1 = match obs1 with Ok v -> v | Error exn -> raise exn in
  let _obs2 = match obs2 with Ok v -> v | Error exn -> raise exn in
  true

(* Common magic constants *)
let rep_count = 25 (* No. of repetitions of the non-deterministic property *)
let retries = 10   (* Additional factor of repetition during shrinking *)
let seq_len = 20   (* max length of the sequential prefix *)
let par_len = 12   (* max length of the parallel cmd lists *)

let stress_test_par ~count ~name =
  let max_gen = 3*count in (* precond filtering may require extra generation: max. 3*count though *)
  QCheck.Test.make ~retries ~max_gen ~count ~name
    (arb_cmds_triple seq_len par_len)
    (fun triple ->
       Util.repeat rep_count stress_prop_par triple) (* 25 times each, then 25 * 10 times when shrinking *)

let _ =
  QCheck.Test.check_exn (
    stress_test_par ~count:2000 ~name:"STM Gc stress test parallel";
  )
