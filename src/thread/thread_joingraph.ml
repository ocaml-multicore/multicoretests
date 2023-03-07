(**
  Generate direct tests of the Thread module's create/join primitives.
  Like [src/domainslib/tast_one_dep.ml]([src/domainslib/tast_one_dep.ml) it does so by generating
  a random, acyclic dependency graph of [create]d [Thread.t]s each waiting
  to [join] with its dependency.
 *)

open QCheck

(* Generates a sparse DAG of join dependencies                        *)
(* Each thread is represented by an array index w/at most 1 dep. each *)
(* This example DAG

     A/0 <--- B/1
      ^.
        \
         `- C/2 <--- D/3

   is represented as: [| None; Some 0; Some 0; Some 2 |]

   Since each thread can only be joined once, A/0 is joined by B/1 (not C/2)
*)
let gen_deps n st =
  let a = Array.make n None in
  for i=1 to n-1 do
    if Gen.bool st then a.(i) <- Some (Gen.int_bound (i-1) st)
  done;
  a

type test_input =
  {
    num_threads  : int;
    dependencies : int option array
  } [@@deriving show { with_path = false }]

let shrink_deps test_input =
  let ls = Array.to_list test_input.dependencies in
  let is = Shrink.list ~shrink:Shrink.(option nil) ls in
  Iter.map
    (fun deps ->
       let len = List.length deps in
       let arr = Array.of_list deps in
       let deps = Array.mapi (fun i j_opt -> match i,j_opt with
            | 0, _
            | _,None -> None
            | _,Some 0 -> Some 0
            | _, Some j ->
              if j<0 || j>=len || j>=i (* ensure reduced dep is valid *)
              then Some ((j + i) mod i)
              else Some j) arr in
       { num_threads=len; dependencies=deps }) is

let arb_deps thread_bound =
  let gen_deps =
    Gen.(int_bound (thread_bound-1) >>= fun num_threads ->
         let num_threads = succ num_threads in
         gen_deps num_threads >>= fun dependencies -> return { num_threads; dependencies }) in
  make ~print:show_test_input ~shrink:shrink_deps gen_deps

(*let thread_id id i = Printf.sprintf "(thread %i, index %i)" id i*)

let is_first_with_dep i dep deps =
  [] = List.filteri (fun j opt -> j < i && opt = Some dep) (Array.to_list deps)

let build_dep_graph test_input f =
  let rec build i thread_acc =
    if i=test_input.num_threads
    then List.rev thread_acc
    else
      let p = (match test_input.dependencies.(i) with
          | None ->
            Thread.create f ()
          | Some dep ->
            Thread.create (fun () ->
                f();
                if is_first_with_dep i dep test_input.dependencies
                then
                  let p' = List.nth thread_acc (i-1-dep) in
                  Thread.join p';
              ) ()) in
      build (i+1) (p::thread_acc)
  in
  build 0 []

(** In this first test each created thread calls [work] - and then optionally join. *)
(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
  else z

let work () =
  for _ = 1 to 100 do
    assert (7 = tak 18 12 6);
  done

let test_tak_work ~thread_bound =
  Test.make ~name:"Thread.create/join - tak work" ~count:100
    (arb_deps thread_bound)
    ((*Util.fork_prop_with_timeout 30*)
    (fun test_input ->
      (*Printf.printf "%s\n%!" (show_test_input test_input);*)
      let ps = build_dep_graph test_input work in
      List.iteri (fun i p -> if not (Array.mem (Some i) test_input.dependencies) then Thread.join p) ps;
      true))

(** In this test each created thread calls [Atomic.incr] - and then optionally join. *)
let test_atomic_work ~thread_bound =
  Test.make ~name:"Thread.create/join - atomic" ~count:500
    (arb_deps thread_bound)
    (fun test_input ->
       let a = Atomic.make 0 in
       let ps = build_dep_graph test_input (fun () -> Atomic.incr a) in
       List.iteri (fun i p ->
           if not (Array.mem (Some i) test_input.dependencies)
           then
             Thread.join p;
         ) ps;
       Atomic.get a = test_input.num_threads)

let bound_tak = if Sys.word_size == 64 then 100 else 16
let bound_atomic = if Sys.word_size == 64 then 250 else 16

;;
QCheck_base_runner.run_tests_main
  [test_tak_work    ~thread_bound:bound_tak;
   test_atomic_work ~thread_bound:bound_atomic
  ]
