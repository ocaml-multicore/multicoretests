module Lin_queue_domain = Lin_domain.Make(Lin_tests_spec_queue)

let count = 10000

let failures = Lin_queue_domain.lin_stats ~count
let () = Printf.printf "Queue Domain %i / %i\n%!" failures count
