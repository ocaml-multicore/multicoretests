(**
  Generate direct tests of the Domain module's spawn/join primitives.
  Like [src/domainslib/tast_one_dep.ml]([src/domainslib/tast_one_dep.ml) it does so by generating
  a random, acyclic dependency graph of [spawn]ed [Domain.t]s each waiting
  to [join] with its dependency.
 *)

open QCheck

(* Generates a sparse DAG of join dependencies                        *)
(* Each domain is represented by an array index w/at most 1 dep. each *)
(* This example DAG

     A/0 <--- B/1
      ^.
        \
         `- C/2 <--- D/3

   is represented as: [| None; Some 0; Some 0; Some 2 |]

   Since each domain can only be joined once, A/0 is joined by B/1 (not C/2)
*)
let gen_deps n st =
  let a = Array.make n None in
  for i=1 to n-1 do
    if Gen.bool st then a.(i) <- Some (Gen.int_bound (i-1) st)
  done;
  a

(* FIXME:
   - Make sparsety a random param - not just a bool in gen_deps
   - negative tests: catch and check expected exception
   - one domain could join several other domains
   - all domains are spawned by main - but joined randomly
     ideally: spawned by random domain and joined by random domain

   only one domain can spawn (but repeatedly) - this gives rise to a spawn tree
   only one domain can join on a domain (after creation)
    - this gives rise to a join tree
    - or a join forest (if there are un-joined domains
*)

type test_input =
  {
    num_domains  : int;
    dependencies : int option array
  }

let pp_test_input par fmt { num_domains; dependencies } =
  let open Util.Pp in
  pp_record par fmt
    [
      pp_field "num_domains" pp_int num_domains;
      pp_field "dependencies" (pp_array (pp_option pp_int)) dependencies;
    ]

let show_test_input = Util.Pp.to_show pp_test_input

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
       { num_domains=len; dependencies=deps }) is

let arb_deps domain_bound =
  let gen_deps =
    Gen.(int_bound (domain_bound-1) >>= fun num_domains ->
         let num_domains = succ num_domains in
         gen_deps num_domains >>= fun dependencies -> return { num_domains; dependencies }) in
  make ~print:show_test_input ~shrink:shrink_deps gen_deps

(*let dom_id id i = Printf.sprintf "(domain %i, index %i)" id i*)

let is_first_with_dep i dep deps =
  [] = List.filteri (fun j opt -> j < i && opt = Some dep) (Array.to_list deps)

let build_dep_graph test_input f =
  let rec build i domain_acc =
    if i=test_input.num_domains
    then List.rev domain_acc
    else
      let p = (match test_input.dependencies.(i) with
          | None ->
            Domain.spawn f
          | Some dep ->
            Domain.spawn (fun () ->
                f();
                if is_first_with_dep i dep test_input.dependencies
                then
                  let p' = List.nth domain_acc (i-1-dep) in
                  (*let src_id = (dom_id (Domain.self () :> int) i) in
                    let tgt_id = (dom_id (Domain.get_id p' :> int) dep) in*)
                  Domain.join p';
                  (*Printf.printf "graph -- %s joining %s success\n%!" src_id tgt_id*)
              )) in
      build (i+1) (p::domain_acc)
  in
  build 0 []

(** In this first test each spawned domain calls [work] - and then optionally join. *)
(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
  else z

let work () =
  for _ = 1 to 100 do
    assert (7 = tak 18 12 6);
  done

let test_tak_work ~domain_bound =
  Test.make ~name:"Domain.spawn/join - tak work" ~count:100
    (arb_deps domain_bound)
    ((*Util.fork_prop_with_timeout 30*)
    (fun test_input ->
      (*Printf.printf "%s\n%!" (show_test_input test_input);*)
      let ps = build_dep_graph test_input work in
      List.iteri (fun i p -> if not (Array.mem (Some i) test_input.dependencies) then Domain.join p) ps;
      true))

(** In this test each spawned domain calls [Atomic.incr] - and then optionally join. *)
let test_atomic_work ~domain_bound =
  Test.make ~name:"Domain.spawn/join - atomic" ~count:500
    (arb_deps domain_bound)
    (fun test_input ->
       let a = Atomic.make 0 in
       let ps = build_dep_graph test_input (fun () -> Atomic.incr a) in
       List.iteri (fun i p ->
           if not (Array.mem (Some i) test_input.dependencies)
           then
             (*let tgt_id = dom_id (Domain.get_id p :> int) i in*)
             Domain.join p;
             (*Printf.printf "main domain %i -- joining %s success\n%!" (Domain.self () :> int) tgt_id*)
         ) ps;
       Atomic.get a = test_input.num_domains)

let bound_tak = if Sys.word_size == 64 then 100 else 8
let bound_atomic = if Sys.word_size == 64 then 250 else 8

;;
QCheck_base_runner.run_tests_main
  [test_tak_work ~domain_bound:bound_tak;
   test_atomic_work ~domain_bound:bound_atomic
  ]
