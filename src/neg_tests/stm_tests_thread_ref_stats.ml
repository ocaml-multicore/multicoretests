open Stm_tests_spec_ref

module RT_int   = STM_thread.Make(RConf_int)   [@alert "-experimental"]
module RT_int64 = STM_thread.Make(RConf_int64) [@alert "-experimental"]

let count = 10000

let rt_int_count = RT_int.agree_stats_conc ~count
let () = Printf.printf "int ref Thread %i / %i\n%!" rt_int_count count

let rt_int64_count = RT_int64.agree_stats_conc ~count
let () = Printf.printf "int64 ref Thread %i / %i\n%!" rt_int64_count count
