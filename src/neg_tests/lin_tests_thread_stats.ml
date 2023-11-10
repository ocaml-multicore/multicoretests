open Lin_tests_common

(** This is a driver of the negative tests over the Thread module *)

module RT_int_thread = Lin_thread.Make(Ref_int_spec) [@alert "-experimental"]
module RT_int64_thread = Lin_thread.Make(Ref_int64_spec) [@alert "-experimental"]
module CLT_int_thread = Lin_thread.Make(CList_spec_int) [@alert "-experimental"]
module CLT_int64_thread = Lin_thread.Make(CList_spec_int64) [@alert "-experimental"]

let count = 10000

let failures = RT_int_thread.lin_stats ~count
let () = Printf.printf "Lin int ref Thread %i / %i\n%!" failures count

let failures = RT_int64_thread.lin_stats ~count
let () = Printf.printf "Lin int64 ref Thread %i / %i\n%!" failures count

let failures = CLT_int_thread.lin_stats ~count
let () = Printf.printf "Lin int CList Thread %i / %i\n%!" failures count

let failures = CLT_int64_thread.lin_stats ~count
let () = Printf.printf "Lin int64 CList Thread %i / %i\n%!" failures count
