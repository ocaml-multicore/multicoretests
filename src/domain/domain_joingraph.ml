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

   is represented as:
      [| {dep=None ...}; {dep=Some 0 ...}; {dep=Some 0 ...}; {dep=Some 2 ...} |]

   Since each domain can only be joined once, A/0 is joined by B/1 (not C/2)
*)

type node =
  {
    dep  : int option;
    work : Work.worktype
  }

type test_input =
  {
    num_domains  : int;
    dependencies : node array
  }

let gen_deps gen_work n st =
  Array.init n
    (fun i ->
       let dep = if i<>0 && Gen.bool st then Some (Gen.int_bound (i-1) st) else None in
       let work = gen_work st in
       { dep; work })

let pp_node par fmt {dep;work} =
  let open Util.Pp in
  pp_record par fmt
    [
      pp_field "dep" (pp_option pp_int) dep;
      pp_field "work" Work.pp_worktype work;
    ]

let pp_test_input par fmt { num_domains; dependencies } =
  let open Util.Pp in
  pp_record par fmt
    [
      pp_field "num_domains" pp_int num_domains;
      pp_field "dependencies" (pp_array pp_node) dependencies;
    ]

let show_test_input = Util.Pp.to_show pp_test_input

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
       { num_domains=len; dependencies=deps }) is

let arb_deps gen_work domain_bound =
  let gen_deps =
    Gen.(int_bound (domain_bound-1) >>= fun num_domains ->
         let num_domains = succ num_domains in
         gen_deps gen_work num_domains >>= fun dependencies -> return { num_domains; dependencies }) in
  make ~print:show_test_input ~shrink:shrink_deps gen_deps

(*let dom_id id i = Printf.sprintf "(domain %i, index %i)" id i*)

let is_first_with_dep i dep deps =
  [] = List.filteri (fun j node -> j < i && node.dep = Some dep) (Array.to_list deps)

let a = Atomic.make 0

let interp_work w = Work.run w a

let build_dep_graph test_input (*f*) =
  let rec build i domain_acc =
    if i=test_input.num_domains
    then List.rev domain_acc
    else
      let p = (match test_input.dependencies.(i).dep with
          | None ->
            Domain.spawn (fun () ->
                interp_work test_input.dependencies.(i).work
              )
          | Some dep ->
            Domain.spawn (fun () ->
                interp_work test_input.dependencies.(i).work;
                (*f();*)
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

let count_incrs test_input =
  Array.fold_left (fun a n -> if n.work = Atomic_incr then 1+a else a) 0 test_input.dependencies

let test_arb_work ~domain_bound =
  Test.make ~name:"Domain.spawn/join" ~count:500
    (arb_deps Work.qcheck_gen domain_bound)
    (fun test_input ->
       Atomic.set a 0;
       let ps = build_dep_graph test_input in
       List.iteri (fun i p ->
           if not (Array.exists (fun n -> n.dep = Some i) test_input.dependencies)
           then
             (*let tgt_id = dom_id (Domain.get_id p :> int) i in*)
             Domain.join p;
             (*Printf.printf "main domain %i -- joining %s success\n%!" (Domain.self () :> int) tgt_id*)
         ) ps;
       Atomic.get a = count_incrs test_input)

let bound_arb = if Sys.word_size == 64 then 250 else 8

;;
QCheck_base_runner.run_tests_main
  [test_arb_work ~domain_bound:bound_arb ]
