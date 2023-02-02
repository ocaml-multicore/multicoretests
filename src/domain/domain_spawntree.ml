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
  | Spawn of cmd list [@@deriving show { with_path = false }]

let gen max_depth max_width =
  let depth_gen = Gen.int_bound max_depth in
  let width_gen = Gen.int_bound max_width in
  Gen.sized_size depth_gen @@ Gen.fix (fun rgen n ->
    match n with
    | 0 -> Gen.oneofl [Incr;Decr]
    | _ ->
      Gen.oneof
        [
          Gen.oneofl [Incr;Decr];
          Gen.map (fun ls -> Spawn ls) (Gen.list_size width_gen (rgen (n/2)))
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

let t ~max_depth ~max_width = Test.make
    ~name:"domain_spawntree - with Atomic"
    ~count:100
    ~retries:10
    (*~print:show_cmd (gen max_depth max_width)*)
    (make ~print:show_cmd ~shrink:shrink_cmd (gen max_depth max_width))

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
;;
QCheck_base_runner.run_tests_main [t ~max_depth:20 ~max_width:10]
