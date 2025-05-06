let cmd_len = 10   (* Length of the generated parallel cmd lists *)
let rep_count = 10 (* No. of repetitions of the non-deterministic property *)
let num_domains = 8

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

let arb_tuple arb_cmd = QCheck.(make Gen.(list_repeat cmd_len arb_cmd.gen))

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

let interp_cmds sut cs = List.map (fun c -> Domain.cpu_relax(); run c sut) cs

let stress_prop_par cmds =
  let sut = ref [] in
  let barrier = Atomic.make num_domains in
  let main () =
    Atomic.decr barrier;
    while Atomic.get barrier <> 0 do Domain.cpu_relax() done;
    Ok (interp_cmds sut cmds)
  in
  let a = Array.init num_domains (fun _ -> Domain.spawn main) in
  let _r = Array.map Domain.join a in
  let ()   = cleanup sut in
  true

let rec repeat n prop input = n<=0 || (prop input && repeat (n-1) prop input)

let stress_test_par =
  QCheck.Test.make ~count:1000 ~name:"STM Gc stress test parallel"
    (arb_tuple arb_cmd)
    (fun tuple -> repeat rep_count stress_prop_par tuple) (* 25 times each *)

let _ = QCheck.Test.check_exn stress_test_par
