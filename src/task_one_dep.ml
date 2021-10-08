(**
  Generate tests of async+await from Domainslib.Task.
  It does so by generating a random, acyclic dependency graph of [async] tasks,
  each [await]ing on its dependency.
 *)

open QCheck
open Domainslib

(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
           else z

let rec repeat n =
  if n <= 0 then 0 else tak 18 12 6 + repeat(n-1)


(* Generates a DAG of dependencies                          *)
(* Each task is represented by an array index w/a deps.list *)
(* This example DAG

     A/0 <--- B/1 <
      ^.           \
        \           \
         `- C/2 <--- D/3

   is represented as: [| []; [0]; [0]; [1;2] |] *)
let gen_dag n st =
  Array.init n (fun i ->
      let deps = ref [] in
      for dep = 0 to i-1 do
        if Gen.bool st then deps := dep :: !deps
      done;
      List.rev !deps)


(* Generates a sparse DAG of dependencies                           *)
(* Each task is represented by an array index w/at most 1 dep. each *)
(* This example DAG

     A/0 <--- B/1
      ^.
        \
         `- C/2 <--- D/3

   is represented as: [| None; Some 0; Some 0; Some 2 |] *)
let gen_deps n st =
  let a = Array.make n None in
  for i=1 to n-1 do
    if Gen.bool st then a.(i) <- Some (Gen.int_bound (i-1) st)
  done;
  a

(* FIXME:
   - Add timeout to fix potential infinite loop
   - Make sparsety a random param - not just a bool in gen_deps
   - Make num_additional_domains a random param
   - Make version with several dependencies based on gen_dag (one await for each)
   - Add a shrinker
   - Repeat test multiple times during shrinking (if need be) to aid with reproducability
 *)
let test bound =
  let gen_pair =
    Gen.(map succ (int_bound bound) >>= fun n -> gen_deps n >>= fun a -> return (n,a)) in
  let print_pair = Print.(pair int (array (option int))) in
  let arb_pair = make ~print:print_pair gen_pair in
  Test.make ~name:"Task.async/await" ~count:10
    arb_pair
    (fun (len,a) ->
       Printf.printf "%s\n%!" (print_pair (len,a));
       let pool = Task.setup_pool ~num_additional_domains:5 in
       let ps =
         let rec build i promise_acc =
           if i=len
           then promise_acc
           else
             let p = (match a.(i) with
                 | None ->
                   Task.async pool (fun () -> repeat 200)
                 | Some dep ->
                   Task.async pool (fun () ->
                       let r = repeat 200 in
                       let other = Task.await pool (List.nth promise_acc (i-1-dep)) in
                       assert (r=other);
                       r)) in
             build (i+1) (p::promise_acc)
         in
         build 0 [] in
       let res = List.fold_left (fun acc p -> assert (acc = Task.await pool p); acc) 1400 ps in
       Task.teardown_pool pool;
       res = 1400)

;;
QCheck_base_runner.run_tests [test 10]
