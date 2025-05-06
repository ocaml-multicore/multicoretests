let cmd_len = 10   (* Length of the generated parallel cmd lists *)
let rep_count = 10 (* No. of repetitions of the non-deterministic property *)
let num_domains = 4

type cmd =
  | Compact
  | PreAllocList of unit list
  | RevList

let arb_cmd =
  QCheck.(make
    Gen.(frequency [
      5, map (fun size -> PreAllocList (List.init size (fun _ -> ()))) QCheck.Gen.nat;
      5, return RevList;
      1, return Compact;
    ]))

let cleanup sut =
  begin
    sut := [];
    Gc.major ()
  end

let unit = ((), print_newline)

let run c sut = match c with (* the pair allocations also help trigger the bug *)
  | Compact        -> (unit, Gc.compact ())
  | PreAllocList l -> (unit, (sut := l)) (*alloc list in parent domain *)
  | RevList        -> (unit, (sut := List.rev !sut)) (*alloc list in child domain *)

let rec gen_cmds arb fuel =
  QCheck.Gen.(if fuel = 0
       then return []
       else
         arb.QCheck.gen >>= fun c ->
         (gen_cmds arb (fuel-1)) >>= fun cs ->
         return (c::cs))

let gen_cmds_size gen size_gen = QCheck.Gen.sized_size size_gen (gen_cmds gen)

let arb_tuple arb_cmd =
  let gen_tuple =
    QCheck.Gen.(
      array_repeat num_domains (gen_cmds_size arb_cmd (return cmd_len))) in
  QCheck.make gen_tuple

let interp_cmds sut cs = List.map (fun c -> Domain.cpu_relax(); run c sut) cs

let stress_prop_par cmds =
  let sut = ref [] in
  let barrier = Atomic.make num_domains in
  let main cmds () =
    Atomic.decr barrier;
    while Atomic.get barrier <> 0 do Domain.cpu_relax() done;
    Ok (interp_cmds sut cmds)
  in
  let a = Array.init num_domains (fun i -> Domain.spawn (main cmds.(i))) in
  let _r = Array.map Domain.join a in
  let ()   = cleanup sut in
  true

let rec repeat n prop input = n<=0 || (prop input && repeat (n-1) prop input)

let stress_test_par =
  QCheck.Test.make ~count:1000 ~name:"STM Gc stress test parallel"
    (arb_tuple arb_cmd)
    (fun tuple -> repeat rep_count stress_prop_par tuple) (* 25 times each *)

let _ = QCheck.Test.check_exn stress_test_par
