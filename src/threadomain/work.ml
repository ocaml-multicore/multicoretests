type worktype = Burn | Tak of int | Gc_minor

let pp_worktype par fmt x =
  let open Util.Pp in
  match x with
  | Burn -> cst0 "Burn" fmt
  | Tak x -> cst1 pp_int "Tak" par fmt x
  | Gc_minor -> cst0 "Gc_minor" fmt

let qcheck2_gen =
  let open QCheck2.Gen in
  frequency
    [(10, pure Burn);
     (10, map (fun i -> Tak i) (int_bound 200));
     (1, pure Gc_minor)]

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
  | Gc_minor -> Gc.minor ()
