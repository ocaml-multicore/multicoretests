(* ********************************************************************** *)
(*                      Tests of in and out channels                      *)
(* ********************************************************************** *)

module IC_domain = Lin_domain.Make(Lin_tests_spec_io.ICConf)
module OC_domain = Lin_domain.Make(Lin_tests_spec_io.OCConf)

let count = 10000

let failures = IC_domain.lin_stats ~count
let () = Printf.printf "In_channel Domain %i / %i\n%!" failures count

let failures = OC_domain.lin_stats ~count
let () = Printf.printf "Out_channel Domain %i / %i\n%!" failures count
