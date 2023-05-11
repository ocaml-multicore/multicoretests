(** This tests the Domain module's spawn/join primitives. *)

(*
 Idea: generate a series of spawn trees:

                   Spawn
                 / |   | \
                /  |   |  \
               /   |   |   \
            Incr Spawn Incr Spawn
                 / | \        |
                /  |  \       |
               /   |   \      |
            Incr Incr Incr  Decr


 Each tree is interpreted over Domain:
  - [Spawn] call [Domain.spawn] for each child
  - [Incr] and [Decr] call [Atomic.incr] and [Atomic.decr], respectively
*)

open QCheck

type cmd =
  | Incr
  | Decr
  (*| Join*)
  | Spawn of cmd list

let rec pp_cmd par fmt x =
  let open Util.Pp in
  match x with
  | Incr -> cst0 "Incr" fmt
  | Decr -> cst0 "Decr" fmt
  | Spawn x -> cst1 (pp_list pp_cmd) "Spawn" par fmt x

let show_cmd = Util.Pp.to_show pp_cmd

let gen max_height max_degree =
  let height_gen = Gen.int_bound max_height in
  let degree_gen = Gen.int_bound max_degree in
  Gen.sized_size height_gen @@ Gen.fix (fun rgen n ->
    match n with
    | 0 -> Gen.oneofl [Incr;Decr]
    | _ ->
      Gen.oneof
        [
          Gen.oneofl [Incr;Decr];
          Gen.map (fun ls -> Spawn ls) (Gen.list_size degree_gen (rgen (n-1)))
        ])

let rec shrink_cmd = function
  | Incr
  | Decr -> Iter.empty
  | Spawn cs ->
    let open Iter in
    (return Incr)
    <+>
    (map (fun cs' -> Spawn cs') (Shrink.list_elems shrink_cmd cs))
    <+>
    (map (fun cs' -> Spawn cs') (Shrink.list_spine cs))

let rec interp s = function
  | Incr -> succ s
  | Decr -> pred s
  | Spawn cs -> List.fold_left (fun s' c -> interp s' c) s cs

let rec dom_interp a = function
  | Incr -> Atomic.incr a
  | Decr -> Atomic.decr a
  | Spawn cs ->
    let ds = List.map (fun c -> Domain.spawn (fun () -> dom_interp a c)) cs in
    List.iter Domain.join ds

let t ~max_height ~max_degree = Test.make
    ~name:"domain_spawntree - with Atomic"
    ~count:100
    ~retries:10
    (*~print:show_cmd (gen max_height max_degree)*)
    (make ~print:show_cmd ~shrink:shrink_cmd (gen max_height max_degree))

    ((*Util.fork_prop_with_timeout 30*) (* forking a fresh process starts afresh, it seems *)
       (fun c ->
         (*Printf.printf "spawns: %i\n%!" (count_spawns c);*)
          (*Printf.printf "%s\n%!" (show_cmd c);*)
          try
            let a = Atomic.make 0 in
            let () = dom_interp a c in
            Atomic.get a = interp 0 c
          with
          | Failure s ->
            if s = "failed to allocate domain"
            then true
            else (Printf.printf "Failure \"%s\"\n%!" s; false)
       ))

let test =
  if Sys.word_size == 64
  then t ~max_height:5 ~max_degree:10
  else t ~max_height:3 ~max_degree:3

;;
QCheck_base_runner.run_tests_main [test]
