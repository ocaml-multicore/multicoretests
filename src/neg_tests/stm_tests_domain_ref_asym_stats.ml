open Stm_tests_spec_ref

module RT_int   = STM_domain.Make(RConf_int)
module RT_int64 = STM_domain.Make(RConf_int64)
;;

let count = 5000

let failures = RT_int.agree_stats_par ~count
let () = Printf.printf "STM int ref test parallel asymmetric     %i / %i\n%!" failures count
let failures = RT_int.agree_stats_par ~count
let () = Printf.printf "STM int64 ref test parallel asymmetric   %i / %i\n%!" failures count
