let cmd_len = 10   (* Length of the generated parallel cmd lists *)
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

let arb_cmd_list = QCheck.(make Gen.(list_repeat cmd_len arb_cmd.gen))

let run c sut = match c with (* the pair allocations also help trigger the bug *)
  | Compact        -> ((), Gc.compact ())
  | PreAllocList l -> ((), (sut := l)) (*alloc list in parent domain *)
  | RevList        -> ((), (sut := List.rev !sut)) (*alloc list in child domain *)

let stress_prop_par cmds =
  let sut = ref [] in
  let barrier = Atomic.make num_domains in
  let main () =
    Atomic.decr barrier;
    while Atomic.get barrier <> 0 do Domain.cpu_relax() done;
    List.map (fun c -> Domain.cpu_relax(); run c sut) cmds
  in
  let a = Array.init num_domains (fun _ -> Domain.spawn main) in
  let _ = Array.map Domain.join a in
  sut := [];
  Gc.major ();
  true

let stress_test_par = QCheck.Test.make arb_cmd_list stress_prop_par

let _ = QCheck.Test.check_exn stress_test_par
