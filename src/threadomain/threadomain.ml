open QCheck2

(* We mix domains and threads. We use the name _node_ for either a
   domain or a thread *)

(** Generate a permutation of [0..sz-1] *)
let permutation sz = Gen.shuffle_a (Array.init sz (fun x -> x))

(** Generate a tree of size nodes
 The tree is represented as an array [a] of integers, [a.(i)] being
 the parent of node [i]. Node [0] is the root of the tree.
 *)
let tree sz =
  let open Gen in
  let rec aux a i =
    if i == 0
    then ( a.(i) <- -1 ;
           pure a )
    else ( let* v = int_bound (i-1) in
           a.(i) <- v ;
           aux a (i-1) )
  in aux (Array.make sz 0) (sz-1)

(** A test of spawn and join

    [spawn_tree] describes which domain/thread should spawn which other
    domains/threads
    [join_permutation] maps nodes to their position in the [join_tree]
    [join_tree] describes which domain/thread should wait on which
    other domains/threads
    [domain_or] describes whether a given node is a domain (true) or a
    thread (false)

    All those arrays should be of the same length, maybe an array of
    tuples would be a better choice, but make harder to read
*)
type spawn_join = {
  spawn_tree:       int array;
  join_permutation: int array;
  join_tree:        int array;
  domain_or:        bool array;
  workload:         Work.worktype array
}

let pp_spawn_join par fmt
    { spawn_tree; join_permutation; join_tree; domain_or; workload } =
  let open Util.Pp in
  pp_record par fmt
    [
      pp_field "spawn_tree" (pp_array pp_int) spawn_tree;
      pp_field "join_permutation" (pp_array pp_int) join_permutation;
      pp_field "join_tree" (pp_array pp_int) join_tree;
      pp_field "domain_or" (pp_array pp_bool) domain_or;
      pp_field "workload" (pp_array Work.pp_worktype) workload;
    ]

let show_spawn_join = Util.Pp.to_show pp_spawn_join

(* Ensure that any domain is higher up in the join tree than all its
   threads, so that we cannot have a thread waiting on its domain even
   indirectly *)
let fix_permutation sz sj =
  let swap arr i j =
    let x = arr.(i) in
    arr.(i) <- arr.(j) ;
    arr.(j) <- x
  in
  let rec dom_of_thd i =
    let candidate = sj.spawn_tree.(i) in
    if candidate = -1 || sj.domain_or.(candidate)
    then candidate
    else dom_of_thd candidate
  in
  for i = 0 to sz-1 do
    if not sj.domain_or.(i) then
      let i' = sj.join_permutation.(i) in
      let d = dom_of_thd i in
      let d' = if d = -1 then d else sj.join_permutation.(d) in
      if d' > i' then swap sj.join_permutation i d
  done ;
  sj

let build_spawn_join sz spawn_tree join_permutation join_tree domain_or workload =
  fix_permutation sz { spawn_tree; join_permutation; join_tree; domain_or; workload }

let gen_spawn_join sz =
  let open Gen in
  build_spawn_join sz
    <$> tree sz <*> permutation sz <*> tree sz
    <*> array_size (pure sz) (frequencyl [(4, false); (1, true)])
    <*> array_size (pure sz) Work.qcheck2_gen

let print_work_module ostr =
  Printf.fprintf ostr "%s\n%!"
 {|
module Work =
struct
  type worktype = Burn of int | Tak of int | Atomic_incr | Gc_minor

  (* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
  let rec tak x y z =
    if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
    else z

  let rec burn l =
    if List.hd l > 12 then ()
    else
      burn (l @ l |> List.map (fun x -> x + 1))

  let run w a =
    match w with
    | Burn i -> burn [i]
    | Tak i ->
      for _ = 1 to i do
        assert (7 = tak 18 12 6);
      done
    | Atomic_incr -> Atomic.incr a
    | Gc_minor -> Gc.minor ()
end
|}

let print_prog ostr =
  Printf.fprintf ostr "%s\n%!"
 {|
let global = Atomic.make 0

type handle =
  | NoHdl
  | DomainHdl of unit Domain.t
  | ThreadHdl of Thread.t

(* All the node handles.
   Since they’ll be used to join, they are stored in join_permutation
   order *)
type handles = {
  handles: handle array;
  available: Semaphore.Binary.t array
}

let join_one hdls i =
  Semaphore.Binary.acquire hdls.available.(i) ;
  ( match hdls.handles.(i) with
    | NoHdl -> failwith "Semaphore acquired but no handle to join"
    | DomainHdl h -> ( Domain.join h ;
                       hdls.handles.(i) <- NoHdl )
    | ThreadHdl h -> ( Thread.join h ;
                       hdls.handles.(i) <- NoHdl ) )

(** In this first test each spawned domain calls [Work.run] - and then optionally join. *)
let rec spawn_one sj hdls i =
  hdls.handles.(sj.join_permutation.(i)) <-
    if sj.domain_or.(i)
    then DomainHdl (Domain.spawn (run_node sj hdls i))
    else ThreadHdl (Thread.create (run_node sj hdls i) ()) ;
  Semaphore.Binary.release hdls.available.(sj.join_permutation.(i))

and run_node sj hdls i () =
  let sz = Array.length sj.spawn_tree in
  (* spawn nodes *)
  for j = i+1 to sz-1 do
    if sj.spawn_tree.(j) == i
    then spawn_one sj hdls j
  done ;
  Work.run sj.workload.(i) global;
  (* join nodes *)
  let i' = sj.join_permutation.(i) in
  for j = i'+1 to sz-1 do
    if sj.join_tree.(j) == i'
    then join_one hdls j
  done

let sz = Array.length sj.spawn_tree

let hdls = { handles = Array.make sz NoHdl;
             available = Array.init sz (fun _ -> Semaphore.Binary.make false) }

let count_incrs sj =
  let count = ref 0 in
  Array.iteri (fun i _ -> if sj.workload.(i)=Work.Atomic_incr then incr count) sj.spawn_tree;
  !count

(* all the nodes should have been joined now *)
let _ =
  Atomic.set global 0;
  spawn_one sj hdls 0;
  join_one hdls 0;
  assert(Array.for_all (fun h -> h = NoHdl) hdls.handles
         && Atomic.get global = count_incrs sj)
|}

let print_type ostr =
  Printf.fprintf ostr {|
type spawn_join = {
  spawn_tree:       int array;
  join_permutation: int array;
  join_tree:        int array;
  domain_or:        bool array;
  workload:         Work.worktype array
}

|}

let compile_prop sj =
  (*assert (0 = Sys.command "rm -f tmp.cmi tmp.cmo tmp.cmx tmp.exe");*)
  let ostr = open_out "tmp.ml" in
  print_work_module ostr;
  print_type ostr;
  Printf.fprintf ostr "let sj = %s\n%!" (show_spawn_join sj);
  print_prog ostr;
  close_out ostr;
(*0 = Sys.command "ocamlopt -I +unix -I +threads -o tmp.exe unix.cmxa threads.cmxa tmp.ml" &&*)
(*0 = Sys.command "ocamlc -I +unix -I +threads -o tmp.exe unix.cma threads.cma tmp.ml" &&
  0 = Sys.command "timeout 10 ./tmp.exe"*)
  0 = Sys.command "timeout 10 ocaml -I +unix -I +threads unix.cma threads.cma tmp.ml"

let nb_nodes =
  let max = 25 (*if Sys.word_size == 64 then 100 else 16*) in
  Gen.int_range 2 max

let main_test = Test.make ~name:"Mash up of threads and domains"
                          ~count:500
                          ~print:show_spawn_join
                          (Gen.sized_size nb_nodes gen_spawn_join)
                          compile_prop
                          (* to debug deadlocks: *)
                          (* (Util.fork_prop_with_timeout 1 run_all_nodes) *)

let _ =
  QCheck_base_runner.run_tests_main [
    main_test
  ]
