(*
usage: z-test  10000 51 10000 44
*)

let convert_arg i =
  try
    float_of_int (int_of_string Sys.argv.(i))
  with
  | Invalid_argument _
  | Failure _ ->
    (print_endline "usage: z-test  10000 51 10000 44"; exit 1)

let trials1 = convert_arg 1
let succ1   = convert_arg 2
let trials2 = convert_arg 3
let succ2   = convert_arg 4

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
