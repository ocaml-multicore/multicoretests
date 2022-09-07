open QCheck

(* We mix domains and threads. We use the name _node_ for either a
   domain or a thread *)

(* The global number of nodes that will be spawn *)
let size = 2 * Domain.recommended_domain_count

let swap arr i j =
  let x = arr.(i) in
  arr.(i) <- arr.(j) ;
  arr.(j) <- x

(** Generate a permutation of [0..size-1] *)
let permutation s =
  let arr = Array.init size (fun x -> x) in
  for i = size - 1 downto 1 do
    swap arr i (Gen.int_bound i s)
  done ;
  arr

(** Generate a tree of size nodes
 The tree is represented as an array [a] of integers, [a.(i)] being
 the parent of node [i]. Node [0] is the root of the tree.
 *)
let tree s =
  let parent i =
    if i == 0
    then -1
    else Gen.int_bound (i-1) s
  in
  Array.init size parent

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
  domain_or:        bool array
} [@@deriving show { with_path = false }]

(* Ensure that any domain is higher up in the join tree than all its
   threads, so that we cannot have a thread waiting on its domain even
   indirectly *)
let fix_permutation sj =
  let rec dom_of_thd i =
    let candidate = sj.spawn_tree.(i) in
    if candidate = -1 || sj.domain_or.(candidate)
    then candidate
    else dom_of_thd candidate
  in
  for i = 0 to size-1 do
    if not sj.domain_or.(i) then
      let i' = sj.join_permutation.(i) in
      let d = dom_of_thd i in
      let d' = if d = -1 then d else sj.join_permutation.(d) in
      if d' > i' then swap sj.join_permutation i d
  done ;
  sj

let build_spawn_join spawn_tree join_permutation join_tree domain_or =
  fix_permutation { spawn_tree; join_permutation; join_tree; domain_or }

let gen_spawn_join =
  let open Gen in
  build_spawn_join <$> tree <*> permutation <*> tree <*> array_size (pure size) bool

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

let rec spawn_one sj hdls i =
  hdls.handles.(sj.join_permutation.(i)) <-
    if sj.domain_or.(i)
    then DomainHdl (Domain.spawn (run_node sj hdls i))
    else ThreadHdl (Thread.create (run_node sj hdls i) ()) ;
  Semaphore.Binary.release hdls.available.(sj.join_permutation.(i))

and run_node sj hdls i () =
  (* spawn nodes *)
  for j = i+1 to size-1 do
    if sj.spawn_tree.(j) == i
    then spawn_one sj hdls j
  done ;
  (* join nodes *)
  let i' = sj.join_permutation.(i) in
  for j = i'+1 to size-1 do
    if sj.join_tree.(j) == i'
    then join_one hdls j
  done

let run_all_nodes sj =
  let hdls = { handles = Array.make size NoHdl;
               available = Array.init size (fun _ -> Semaphore.Binary.make false) } in
  spawn_one sj hdls 0;
  join_one hdls 0;
  (* all the nodes should have been joined now *)
  Array.for_all (fun h -> h = NoHdl) hdls.handles

let main_test = Test.make ~name:"Mash up of threads and domains"
                          ~count:1000
                          (make ~print:show_spawn_join gen_spawn_join)
                          (Util.fork_prop_with_timeout 1 run_all_nodes)

let _ =
  Util.set_ci_printing () ;
  QCheck_base_runner.run_tests_main [
    main_test
  ]
