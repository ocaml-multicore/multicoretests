(* parallel stress tests of the GC with explicit Gc invocations *)

module Spec =
struct
  type cmd =
    | Set_minor_heap_size_2048
    | Compact
    | PreAllocList of int * unit list
    | RevList of int

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

  let arb_cmd = QCheck.(make (Gen.frequency gc_cmds))

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

  type 'a ty = ..

  type _ ty +=
  | Unit : unit ty

  type 'a ty_show = 'a ty * ('a -> string)

  let unit = (Unit, QCheck.Print.unit)

  type res = Res : 'a ty_show * 'a -> res

  let run c sut = match c with
    | Set_minor_heap_size_2048 -> Res (unit, Gc.set { orig_control with minor_heap_size = 2048 })
    | Compact     -> Res (unit, Gc.compact ())
    | PreAllocList (i,l) -> Res (unit, sut.(i) <- l) (*alloc list in parent domain in test-input*)
    | RevList i -> Res (unit, sut.(i) <- List.rev sut.(i)) (*alloc list at test runtime*)
end


let rec gen_cmds arb fuel =
  QCheck.Gen.(if fuel = 0
       then return []
       else
         arb.QCheck.gen >>= fun c ->
         (gen_cmds arb (fuel-1)) >>= fun cs ->
         return (c::cs))

let gen_cmds_size gen size_gen = QCheck.Gen.sized_size size_gen (gen_cmds gen)

let arb_triple seq_len par_len arb_cmd =
  let seq_pref_gen = gen_cmds_size arb_cmd (QCheck.Gen.int_bound seq_len) in
  let gen_triple =
    QCheck.Gen.(seq_pref_gen >>= fun seq_pref ->
         int_range 2 (2*par_len) >>= fun dbl_plen ->
         let par_len1 = dbl_plen/2 in
         let par_gen1 = gen_cmds_size arb_cmd (return par_len1) in
         let par_gen2 = gen_cmds_size arb_cmd (return (dbl_plen - par_len1)) in
         triple (return seq_pref) par_gen1 par_gen2) in
  QCheck.make gen_triple

let interp_sut_res sut cs =
  let cs_arr = Array.of_list cs in
  let res_arr = Array.map (fun c -> Domain.cpu_relax(); Spec.run c sut) cs_arr in
  List.combine cs (Array.to_list res_arr)

let run_par seq_pref cmds1 cmds2 =
  let sut = Spec.init_sut () in
  let pref_obs = interp_sut_res sut seq_pref in
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
  let obs1 = match obs1 with Ok v -> v | Error exn -> raise exn in
  let obs2 = match obs2 with Ok v -> v | Error exn -> raise exn in
  pref_obs, obs1, obs2

let stress_prop_par (seq_pref,cmds1,cmds2) =
  let _ = run_par seq_pref cmds1 cmds2 in
  true

(* Common magic constants *)
let rep_count = 25 (* No. of repetitions of the non-deterministic property *)
let retries = 10   (* Additional factor of repetition during shrinking *)
let seq_len = 20   (* max length of the sequential prefix *)
let par_len = 12   (* max length of the parallel cmd lists *)

let stress_test_par ~count ~name =
  let max_gen = 3*count in (* precond filtering may require extra generation: max. 3*count though *)
  QCheck.Test.make ~retries ~max_gen ~count ~name
    (arb_triple seq_len par_len Spec.arb_cmd)
    (fun triple ->
       Util.repeat rep_count stress_prop_par triple) (* 25 times each, then 25 * 10 times when shrinking *)

let _ =
  QCheck.Test.check_exn (
    stress_test_par ~count:2000 ~name:"STM Gc stress test parallel";
  )
