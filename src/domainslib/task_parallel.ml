open QCheck
open Domainslib

(** draft tests of Task.parallel_* *)
(*
let print_array a =
  let b = Buffer.create 25 in
  Buffer.add_string b "[|";
  Array.iter (fun elem -> Buffer.add_string b (string_of_int elem ^ "; ")) a;
  Buffer.add_string b "|]";
  Buffer.contents b

let scan_task num_doms array_size =
  let pool = Task.setup_pool ~num_domains:num_doms () in
  let a = Task.run pool (fun () -> Task.parallel_scan pool (+) (Array.make array_size 1)) in
  Task.teardown_pool pool;
  a
*)

let count = 500

let test_parallel_for =
  Test.make ~name:"test Task.parallel_for" ~count
    (triple (int_bound 10) small_nat small_nat)
    (fun (num_domains,array_size,chunk_size) ->
       (*Printf.printf "(%i,%i)\n%!" num_domains array_size;*)
       let pool = Task.setup_pool ~num_domains () in
       let res = Task.run pool (fun () ->
           let a = Atomic.make 0 in
           Task.parallel_for ~chunk_size ~start:0 ~finish:(array_size-1) ~body:(fun _ -> Atomic.incr a) pool;
           Atomic.get a) in
       Task.teardown_pool pool;
       res = array_size)

let test_parallel_for_reduce =
  Test.make ~name:"test Task.parallel_for_reduce" ~count
    (triple (int_bound 10) small_nat small_nat)
    (fun (num_domains,array_size,chunk_size) ->
       (*Printf.printf "(%i,%i,%i)\n%!" num_domains array_size chunk_size;*)
       let pool = Task.setup_pool ~num_domains () in
       let res = Task.run pool (fun () ->
           Task.parallel_for_reduce ~chunk_size ~start:0 ~finish:(array_size-1) ~body:(fun _ -> 1) pool (+) 0) in
       Task.teardown_pool pool;
       res = array_size)

let test_parallel_scan =
  Test.make ~name:"test Task.parallel_scan" ~count
    (pair (int_bound 10) small_nat)
    (fun (num_domains,array_size) ->
       (*Printf.printf "(%i,%i)\n%!" num_domains array_size;*)
       let pool = Task.setup_pool ~num_domains () in
       let a = Task.run pool (fun () -> Task.parallel_scan pool (+) (Array.make array_size 1)) in
       Task.teardown_pool pool;
       a = Array.init array_size (fun i -> i + 1))
;;
QCheck_runner.run_tests_main [
  test_parallel_for;
  test_parallel_for_reduce;
  test_parallel_scan;
]
