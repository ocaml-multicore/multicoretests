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

type worktype = Burn | Tak of int

let pp_worktype par fmt x =
  let open Util.Pp in
  match x with Burn -> cst0 "Burn" fmt | Tak x -> cst1 pp_int "Tak" par fmt x

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
  workload:         worktype array
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
      pp_field "workload" (pp_array pp_worktype) workload;
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

let worktype =
  let open Gen in
  oneof [pure Burn; map (fun i -> Tak i) (int_bound 200)]

let gen_spawn_join sz =
  let open Gen in
  build_spawn_join sz
    <$> tree sz <*> permutation sz <*> tree sz
    <*> array_size (pure sz) (frequencyl [(4, false); (1, true)])
    <*> array_size (pure sz) worktype

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

let global = Atomic.make 0

let join_one hdls i =
  Semaphore.Binary.acquire hdls.available.(i) ;
  ( match hdls.handles.(i) with
    | NoHdl -> failwith "Semaphore acquired but no handle to join"
    | DomainHdl h -> ( Domain.join h ;
                       hdls.handles.(i) <- NoHdl )
    | ThreadHdl h -> ( Thread.join h ;
                       hdls.handles.(i) <- NoHdl ) )

(** In this first test each spawned domain calls [work] - and then optionally join. *)
(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
  else z

let rec burn l =
  if List.hd l > 12 then ()
  else
    burn (l @ l |> List.map (fun x -> x + 1))

let work w =
  match w with
  | Burn -> burn [8]
  | Tak i ->
    for _ = 1 to i do
      assert (7 = tak 18 12 6);
    done

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
  Atomic.incr global ;
  work sj.workload.(i) ;
  (* join nodes *)
  let i' = sj.join_permutation.(i) in
  for j = i'+1 to sz-1 do
    if sj.join_tree.(j) == i'
    then join_one hdls j
  done

let run_all_nodes sj =
  Atomic.set global 0 ;
  let sz = Array.length sj.spawn_tree in
  let hdls = { handles = Array.make sz NoHdl;
               available = Array.init sz (fun _ -> Semaphore.Binary.make false) } in
  spawn_one sj hdls 0;
  join_one hdls 0;
  (* all the nodes should have been joined now *)
  Array.for_all (fun h -> h = NoHdl) hdls.handles
   && Atomic.get global = sz

let nb_nodes =
  let max = if Sys.word_size == 64 then 100 else 16 in
  Gen.int_range 2 max

let main_test = Test.make ~name:"Mash up of threads and domains"
                          ~count:500
                          ~print:show_spawn_join
                          (Gen.sized_size nb_nodes gen_spawn_join)
                          run_all_nodes
                          (* to debug deadlocks: *)
                          (* (Util.fork_prop_with_timeout 1 run_all_nodes) *)

let _ =
  QCheck_base_runner.run_tests_main [
    main_test
  ]
