open Lin_internal_tests_common

(** This is a driver of the negative CList tests over the Thread module *)

module CLT_int_thread = Lin_thread.Make_internal(CLConf (Int)) [@alert "-internal"]
module CLT_int64_thread = Lin_thread.Make_internal(CLConf (Int64)) [@alert "-internal"]

let count = 10000

let failures = CLT_int_thread.lin_stats ~count
let () = Printf.printf "Lin.Internal int CList Thread %i / %i\n%!" failures count

let failures = CLT_int64_thread.lin_stats ~count
let () = Printf.printf "Lin.Internal int64 CList Thread %i / %i\n%!" failures count
