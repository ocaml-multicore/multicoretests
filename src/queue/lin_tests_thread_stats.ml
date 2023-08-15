module Lin_queue_thread = Lin_thread.Make(Lin_tests_spec_queue) [@alert "-experimental"]
module Lin_queue_domain = Lin_domain.Make(Lin_tests_spec_queue)

let count = 10000

let failures = Lin_queue_thread.lin_stats ~count
let () = Printf.printf "Queue Thread %i / %i\n%!" failures count

let failures = Lin_queue_domain.lin_stats ~count
let () = Printf.printf "Queue Domain %i / %i\n%!" failures count
