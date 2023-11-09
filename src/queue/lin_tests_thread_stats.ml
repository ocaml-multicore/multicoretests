module Lin_queue_thread = Lin_thread.Make(Lin_tests_spec_queue) [@alert "-experimental"]

let count = 10000

let failures = Lin_queue_thread.lin_stats ~count
let () = Printf.printf "Queue Thread %i / %i\n%!" failures count
