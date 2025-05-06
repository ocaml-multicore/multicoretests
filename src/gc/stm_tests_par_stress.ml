(* parallel stress tests of the GC with explicit Gc invocations *)

type cmd =
  | Compact
  | PreAllocList of unit list
  | RevList

let gc_cmds =
  let open QCheck in
  let list_gen = Gen.map (fun l -> List.init l (fun _ -> ())) Gen.nat in
  Gen.([
      5, map (fun list -> PreAllocList (list)) list_gen;
      5, return RevList;
      1, return Compact;
    ])

let arb_cmd = QCheck.(make (Gen.frequency gc_cmds))

let init_sut () = ref []

let cleanup sut =
  begin
    sut := [];
    Gc.major ()
  end

let unit = ((), QCheck.Print.unit)

let run c sut = match c with (* the pair allocations also help trigger the bug *)
  | Compact        -> (unit, Gc.compact ())
  | PreAllocList l -> (unit, (sut := l)) (*alloc list in parent domain in test-input*)
  | RevList        -> (unit, (sut := List.rev !sut)) (*alloc list at test runtime*)

let rec gen_cmds arb fuel =
  QCheck.Gen.(if fuel = 0
       then return []
       else
         arb.QCheck.gen >>= fun c ->
         (gen_cmds arb (fuel-1)) >>= fun cs ->
         return (c::cs))

let gen_cmds_size gen size_gen = QCheck.Gen.sized_size size_gen (gen_cmds gen)

let arb_tuple seq_len par_len arb_cmd =
  let seq_pref_gen = gen_cmds_size arb_cmd (QCheck.Gen.int_bound seq_len) in
  let gen_triple =
    QCheck.Gen.(seq_pref_gen >>= fun seq_pref ->
         let par_gen1 = gen_cmds_size arb_cmd (int_range 1 par_len) in
         let par_gen2 = gen_cmds_size arb_cmd (int_range 1 par_len) in
         let par_gen3 = gen_cmds_size arb_cmd (int_range 1 par_len) in
         quad (return seq_pref) par_gen1 par_gen2 par_gen3) in
  QCheck.make gen_triple

let interp_sut_res sut cs =
  let cs_arr = Array.of_list cs in
  Array.map (fun c -> Domain.cpu_relax(); run c sut) cs_arr

let run_par seq_pref cmds1 cmds2 cmds3 =
  let sut = init_sut () in
  let pref_obs = interp_sut_res sut seq_pref in
  let barrier = Atomic.make 3 in
  let main cmds () =
    Atomic.decr barrier;
    while Atomic.get barrier <> 0 do Domain.cpu_relax() done;
    try Ok (interp_sut_res sut cmds) with exn -> Error exn
  in
  let dom1 = Domain.spawn (main cmds1) in
  let dom2 = Domain.spawn (main cmds2) in
  let dom3 = Domain.spawn (main cmds3) in
  let obs1 = Domain.join dom1 in
  let obs2 = Domain.join dom2 in
  let obs3 = Domain.join dom3 in
  let ()   = cleanup sut in
  pref_obs, obs1, obs2, obs3

let stress_prop_par (seq_pref,cmds1,cmds2,cmds3) =
  let _ = run_par seq_pref cmds1 cmds2 cmds3 in
  true

(* Common magic constants *)
let rep_count = 10 (* No. of repetitions of the non-deterministic property *)
let seq_len = 20   (* max length of the sequential prefix *)
let par_len = 12   (* max length of the parallel cmd lists *)

let rec repeat n prop = fun input ->
  if n<0 then failwith "repeat: negative repetition count";
  if n=0
  then true
  else prop input && repeat (n-1) prop input

let stress_test_par =
  QCheck.Test.make ~count:1000 ~name:"STM Gc stress test parallel"
    (arb_tuple seq_len par_len arb_cmd)
    (fun tuple -> repeat rep_count stress_prop_par tuple) (* 25 times each *)

let _ = QCheck.Test.check_exn stress_test_par
