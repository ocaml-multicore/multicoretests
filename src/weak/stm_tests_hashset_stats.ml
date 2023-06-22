(** parallel STM stats of Weak hashsets *)

module WeakHashsetSTM_dom = STM_domain.Make(Stm_tests_spec_hashset.WHSConf)

let count = 10000

let failures = WeakHashsetSTM_dom.agree_stats_par ~count
let () = Printf.printf "Weak Hashset %i / %i\n%!" failures count
