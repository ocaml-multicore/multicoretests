(**
  Generate direct tests of the Thread module's create/join primitives.
  It does so by generating a random, acyclic dependency graph of
  [create]d [Thread.t]s each waiting to [join] with its dependency.
 *)

open QCheck

(* Generates a sparse DAG of join dependencies                        *)
(* Each thread is represented by record with an optional array index
   to model at most 1 dependency each *)
(* This example DAG

     A/0 <--- B/1
      ^.
        \
         `- C/2 <--- D/3

   is represented as:
      [| {dep=None ...}; {dep=Some 0 ...}; {dep=Some 0 ...}; {dep=Some 2 ...} |]

   Since each thread can only be joined once, A/0 is joined by B/1 (not C/2)
*)

type work_kind = Atomic_incr | Tak | Gc_minor

type node =
  {
    dep  : int option;
    work : work_kind
  }

type test_input =
  {
    num_threads  : int;
    dependencies : node array
  }


let gen_deps gen_work n st =
  Array.init n
    (fun i ->
       let dep = if i<>0 && Gen.bool st then Some (Gen.int_bound (i-1) st) else None in
       let work = gen_work st in
       { dep; work })

let show_work_kind w = match w with
  | Atomic_incr -> "Atomic_incr"
  | Tak -> "Tak"
  | Gc_minor -> "Gc_minor"

let pp_work_kind = Util.Pp.of_show show_work_kind

let pp_node par fmt {dep;work} =
  let open Util.Pp in
  pp_record par fmt
    [
      pp_field "dep" (pp_option pp_int) dep;
      pp_field "work" pp_work_kind work;
    ]

let pp_test_input par fmt { num_threads; dependencies } =
  let open Util.Pp in
  pp_record par fmt
    [
      pp_field "num_threads" pp_int num_threads;
      pp_field "dependencies" (pp_array pp_node) dependencies;
    ]

let show_test_input = Util.Pp.to_show pp_test_input

let shrink_node n = Iter.map (fun opt -> { n with dep = opt}) (Shrink.(option nil) n.dep)
let shrink_deps test_input =
  let ls = Array.to_list test_input.dependencies in
  let is = Shrink.list ~shrink:shrink_node ls in
  Iter.map
    (fun deps ->
       let len = List.length deps in
       let arr = Array.of_list deps in
       let deps = Array.mapi (fun i j_node ->
           let dep = match i,j_node.dep with
             | 0, _
             | _,None -> None
             | _,Some 0 -> Some 0
             | _, Some j ->
               if j<0 || j>=len || j>=i (* ensure reduced dep is valid *)
               then Some ((j + i) mod i)
               else Some j in
           { j_node with dep }) arr in
       { num_threads=len; dependencies=deps }) is

let arb_deps gen_work thread_bound =
  let gen_deps =
    Gen.(int_bound (thread_bound-1) >>= fun num_threads ->
         let num_threads = succ num_threads in
         gen_deps gen_work num_threads >>= fun dependencies -> return { num_threads; dependencies }) in
  make ~print:show_test_input ~shrink:shrink_deps gen_deps

(*let thread_id id i = Printf.sprintf "(thread %i, index %i)" id i*)

let is_first_with_dep i dep deps =
  [] = List.filteri (fun j node -> j < i && node.dep = Some dep) (Array.to_list deps)

(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
  else z

let work () =
  for _ = 1 to 100 do
    assert (7 = tak 18 12 6);
  done

let a = Atomic.make 0

let interp_work w = match w with
  | Atomic_incr -> Atomic.incr a
  | Tak -> work ()
  | Gc_minor -> Gc.minor ()

let build_dep_graph test_input =
  let rec build i thread_acc =
    if i=test_input.num_threads
    then List.rev thread_acc
    else
      let p = (match test_input.dependencies.(i).dep with
          | None ->
            Thread.create (fun () ->
                interp_work test_input.dependencies.(i).work
              ) ()
          | Some dep ->
            Thread.create (fun () ->
                interp_work test_input.dependencies.(i).work;
                (*f();*)
                if is_first_with_dep i dep test_input.dependencies
                then
                  let p' = List.nth thread_acc (i-1-dep) in
                  Thread.join p';
              ) ()) in
      build (i+1) (p::thread_acc)
  in
  build 0 []

let test_arb_work ~thread_bound =
  Test.make ~name:"Thread.create/join" ~count:100
    (arb_deps (Gen.frequencyl [(10,Atomic_incr);
                               (10,Tak);
                               (1,Gc_minor)]) thread_bound)
    (fun test_input ->
       Atomic.set a 0;
       let ps = build_dep_graph test_input in
       List.iteri
         (fun i p ->
            if not (Array.exists (fun n -> n.dep = Some i) test_input.dependencies)
            then Thread.join p) ps;
       Atomic.get a
       = Array.fold_left (fun a n -> if n.work = Atomic_incr then 1+a else a) 0 test_input.dependencies)

let bound_arb = if Sys.word_size == 64 then 100 else 16

;;
QCheck_base_runner.run_tests_main
  [test_arb_work    ~thread_bound:bound_arb; ]
