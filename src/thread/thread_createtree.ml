(** This tests the Thread module's create/join primitives. *)

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
  | Create of cmd list

let rec pp_cmd par fmt x =
  let open Util.Pp in
  match x with
  | Incr -> cst0 "Incr" fmt
  | Decr -> cst0 "Decr" fmt
  | Create x -> cst1 (pp_list pp_cmd) "Create" par fmt x

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
          Gen.map (fun ls -> Create ls) (Gen.list_size degree_gen (rgen (n-1)))
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

let t ~max_height ~max_degree = Test.make
    ~name:"thread_createtree - with Atomic"
    ~count:1000
    ~retries:100
    (make ~print:show_cmd ~shrink:shrink_cmd (gen max_height max_degree))
    (fun c ->
       (*Printf.printf "%s\n%!" (show_cmd c);*)
       let a = Atomic.make 0 in
       let () = thread_interp a c in
       Atomic.get a = interp 0 c)

let test =
  if Sys.word_size == 64
  then t ~max_height:5 ~max_degree:10
  else t ~max_height:3 ~max_degree:3

;;
QCheck_base_runner.run_tests_main [test]
