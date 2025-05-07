let cmd_len = 10   (* Length of the generated parallel cmd lists *)
let num_domains = 8
let list_size = 50

type cmd =
  | Compact
  | PreAllocList of unit list
  | RevList

let gen_cmd rs = match Random.State.int rs 7 with (* weighted generation *)
   | 0 | 1 | 2 -> PreAllocList (List.init list_size (fun _ -> ()))
   | 3 | 4 | 5 -> RevList
   | _         -> Compact

let gen_cmd_list rs = List.init cmd_len (fun _ -> gen_cmd rs)

let run c sut = match c with
  | Compact        -> Gc.compact ()
  | PreAllocList l -> sut := l          (*alloced list in parent domain *)
  | RevList        -> sut := List.rev !sut (*alloc list in child domain *)

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

let rec loop gen prop count rs =
  count <= 0
  || let cmds = gen rs in
     prop cmds && loop gen prop (count-1) rs

let _ =
  let rs = Random.State.make_self_init () in
  loop gen_cmd_list stress_prop_par 100 rs
