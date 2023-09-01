type worktype = Burn | Tak of int

let pp_worktype par fmt x =
  let open Util.Pp in
  match x with Burn -> cst0 "Burn" fmt | Tak x -> cst1 pp_int "Tak" par fmt x

let qcheck2_gen =
  let open QCheck2.Gen in
  oneof [pure Burn; map (fun i -> Tak i) (int_bound 200)]

(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
  else z

let rec burn l =
  if List.hd l > 12 then ()
  else
    burn (l @ l |> List.map (fun x -> x + 1))

let run w =
  match w with
  | Burn -> burn [8]
  | Tak i ->
    for _ = 1 to i do
      assert (7 = tak 18 12 6);
    done
