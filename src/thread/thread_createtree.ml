(** This tests the Thread module's create/join primitives. *)

(* let max_threads = 128 (1* for now this matches the internal `Max_domain` C value *1) *)
(*
 Idea: generate a series of spawn trees:

                  Create
                 / |   | \
                /  |   |  \
               /   |   |   \
           Incr Create Incr Create
                 / | \        |
                /  |  \       |
               /   |   \      |
            Incr Incr Incr  Decr


 Each tree is interpreted over Thread:
  - [Create] call [Thread.create] for each child
  - [Incr] and [Decr] call [Atomic.incr] and [Atomic.decr], respectively
*)

open QCheck

type cmd =
  | Incr
  | Decr
  (*| Join*)
  | Create of cmd list [@@deriving show { with_path = false }]

(* Counts the total number of [Create]s in a tree *)
(* let rec count_creates = function *)
(*   | Incr *)
(*   | Decr -> 0 *)
(*   | Create cs -> *)
(*     List.length cs + List.fold_left (+) 0 (List.map count_creates cs) *)

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
          Gen.map (fun ls -> Create ls) (Gen.list_size width_gen (rgen (n/2)))
        ])

let rec shrink_cmd = function
  | Incr
  | Decr -> Iter.empty
  | Create cs ->
    let open Iter in
    (return Incr)
    <+>
    (map (fun cs' -> Create cs') (Shrink.list_elems shrink_cmd cs))
    <+>
    (map (fun cs' -> Create cs') (Shrink.list_spine cs))

let rec interp s = function
  | Incr -> succ s
  | Decr -> pred s
  | Create cs -> List.fold_left (fun s' c -> interp s' c) s cs

let rec thread_interp a = function
  | Incr -> Atomic.incr a
  | Decr -> Atomic.decr a
  | Create cs ->
    let ts = List.map (fun c -> Thread.create (fun () -> thread_interp a c) ()) cs in
    List.iter Thread.join ts

let t ~max_depth ~max_width = Test.make
    ~name:"thread_createtree - with Atomic"
    ~count:1000
    ~retries:100
    (make ~print:show_cmd ~shrink:shrink_cmd (gen max_depth max_width))
    (fun c ->
       (*Printf.printf "creates: %i\n%!" (count_creates c);*)
       (*Printf.printf "%s\n%!" (show_cmd c);*)
       let a = Atomic.make 0 in
       let () = thread_interp a c in
       Atomic.get a = interp 0 c)
;;
QCheck_base_runner.run_tests_main [t ~max_depth:20 ~max_width:10]
