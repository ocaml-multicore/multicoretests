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

let work () =
  for _ = 1 to 200 do
    assert (7 = tak 18 12 6);
  done

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
   - Make sparsety a random param - not just a bool in gen_deps
   - Test Task.{parallel_for, parallel_for_reduce, parallel_scan}  *)

type test_input =
  {
    num_domains  : int;
    length       : int;
    dependencies : int option array
  } [@@deriving show { with_path = false }]

(* an older, more ambitious shrinker *)
(*
let rec shrink_deps i ((len,deps) as pair) =
  if len = 0 || i>=len then Iter.empty
  else
    let front = Array.sub deps 0 i in
    let back = Array.sub deps (i+1) (len - (i+1)) in
    let adjust_indices a = (* adjust dependencies for removed entry *)
      Array.map (function
          | None   -> None
          | Some j ->
            if i=j then None else (*old dependency was just removed in shrink candidate *)
            if j<i then Some j else Some (j-1)) a in
    let without_some deps =
      (if deps.(i) = None
       then Iter.empty
       else let deps' = Array.copy deps in deps'.(i) <- None; Iter.return (len,deps')) in
    Iter.append
      (Iter.return (len-1, Array.append front (adjust_indices back)))
      (Iter.append
         (without_some deps)
         (shrink_deps (i+1) pair))
*)
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
       { test_input with length=len; dependencies=deps }) is

let arb_deps domain_bound promise_bound =
  let gen_deps =
    Gen.(pair (int_bound (domain_bound-1)) (int_bound promise_bound) >>= fun (num_domains,length) ->
         let num_domains = succ num_domains in
         let length = succ length in
         gen_deps length >>= fun dependencies -> return { num_domains; length; dependencies }) in
  let shrink_input input =
    Iter.append
      (Iter.map (fun doms' -> { input with num_domains = doms' }) (Shrink.int input.num_domains))
      (shrink_deps input) in
  make ~print:show_test_input ~shrink:shrink_input gen_deps

let build_dep_graph pool test_input =
  let len = test_input.length in
  let deps = test_input.dependencies in
  let rec build i promise_acc =
    if i=len
    then promise_acc
    else
      let p = (match deps.(i) with
          | None ->
            Task.async pool work
          | Some dep ->
            Task.async pool (fun () ->
                work();
                Task.await pool (List.nth promise_acc (i-1-dep)))) in
      build (i+1) (p::promise_acc)
  in
  build 0 []

let test ~domain_bound ~promise_bound =
  Test.make ~name:"Task.async/await" ~count:100
  (*Non_det.Test.make ~repeat:50 ~name:"Task.async/await" ~count:100*)
    (arb_deps domain_bound promise_bound)
    (Util.fork_prop_with_timeout 10
    (fun test_input ->
       (*Printf.printf "%s\n%!" (show_test_input test_input);*)
       let pool = Task.setup_pool ~num_additional_domains:test_input.num_domains () in
       let ps = build_dep_graph pool test_input in
       let _res = List.iter (fun p -> Task.await pool p) ps in
       Task.teardown_pool pool;
       true))
;;
QCheck_base_runner.run_tests_main [test ~domain_bound:8 ~promise_bound:10]
(*Non_det.QCheck_runner.run_tests [test ~domain_bound:8 ~promise_bound:10]*)
