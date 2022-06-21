(*
usage: z-test  10000 51 10000 44
*)

let print_usage_and_exit () =
  (print_endline "usage: z-test  10000 51 10000 44"; exit 1)

let check_and_convert_trials s =
  try
    let trials = int_of_string s in
    if trials < 0
    then (Printf.printf "error: negative trial count: %i\n" trials; print_usage_and_exit ())
    else if trials = 0
    then (Printf.printf "error: trial count is zero: %i\n" trials; print_usage_and_exit ())
    else trials
  with
  | Failure _ -> print_usage_and_exit ()

let check_and_convert_succ s =
  try
    let succ = int_of_string s in
    if succ < 0
    then (Printf.printf "error: negative success count: %i\n" succ; print_usage_and_exit ())
    else succ
  with
  | Failure _ -> print_usage_and_exit ()

let check_consistency trials succ =
  if trials < succ
  then (Printf.printf "error: more successes than trials! %i < %i\n" trials succ; print_usage_and_exit ())

;;
if Array.length Sys.argv <> 5 then print_usage_and_exit ()
;;
let trials1 = check_and_convert_trials Sys.argv.(1)
let succ1   = check_and_convert_succ Sys.argv.(2)
let trials2 = check_and_convert_trials Sys.argv.(3)
let succ2   = check_and_convert_succ Sys.argv.(4)
;;
check_consistency trials1 succ1;;
check_consistency trials2 succ2;;

let trials1 = float_of_int trials1
let succ1   = float_of_int succ1
let trials2 = float_of_int trials2
let succ2   = float_of_int succ2

let p1 = succ1 /. trials1
let p2 = succ2 /. trials2

let p = (succ1 +. succ2) /. (trials1 +. trials2)
let z = (p1 -. p2) /. sqrt (p *. (1. -. p) *. ( (1. /. trials1) +. (1. /. trials2) ))

let z_alpha2 = 1.96
;;
print_endline "z-test of two proportions";;
Printf.printf "z = %f\n%!" z;;
Printf.printf "Is |z| = |%f| > z_alpha2 = %f ?\n" z z_alpha2;;
if Float.abs z > z_alpha2
then Printf.printf "Yes, null hypothesis rejected\n%!"
else Printf.printf "No, failed to reject null hypothesis\n%!"
