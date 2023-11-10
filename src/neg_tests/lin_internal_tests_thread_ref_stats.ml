open Lin_internal_tests_common

(** This is a driver of the negative ref tests over the Thread module *)

module RT_int_thread = Lin_thread.Make_internal(RConf_int) [@alert "-internal"]
module RT_int64_thread = Lin_thread.Make_internal(struct
    include RConf_int64
    let shrink_cmd = QCheck.Shrink.nil
  end ) [@alert "-internal"]

let count = 10000

let failures = RT_int_thread.lin_stats ~count
let () = Printf.printf "Lin.Internal int ref Thread %i / %i\n%!" failures count

let failures = RT_int64_thread.lin_stats ~count
let () = Printf.printf "Lin.Internal int64 ref Thread %i / %i\n%!" failures count
