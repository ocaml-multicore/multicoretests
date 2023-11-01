type worktype = Burn of int | Tak of int | Atomic_incr | Gc_minor

let pp_worktype par fmt x =
  let open Util.Pp in
  match x with
  | Burn i -> cst1 pp_int "Burn" par fmt i
  | Tak x -> cst1 pp_int "Tak" par fmt x
  | Atomic_incr -> cst0 "Atomic_incr" fmt
  | Gc_minor -> cst0 "Gc_minor" fmt

let candidates_gens =
  let open QCheck.Gen in
  [| (15, map (fun i -> Burn i) (int_range 8 12));
     (10, map (fun i -> Tak i) (int_bound 200));
     (10, pure Atomic_incr) |]

(* Generate a subset of the above and
   then include Gc_minor with a 50% chance on top *)
let qcheck_gen =
  let open QCheck.Gen in
  (int_range 1 (Array.length candidates_gens)) >>= fun num_cands ->
  array_subset num_cands candidates_gens >>= fun selection ->
  let gens = Array.to_list selection in
  bool >>= fun incl_gc_minor ->
  let gens =
    if incl_gc_minor
    then (1, pure Gc_minor)::gens
    else gens in
  frequency gens

let qcheck2_gen =
  let open QCheck2.Gen in
  frequency
    [(15, map (fun i -> Burn i) (int_range 8 12));
     (10, map (fun i -> Tak i) (int_bound 200));
     (10, pure Atomic_incr);
     (1, pure Gc_minor)]

(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
  else z

let rec burn l =
  if List.hd l > 12 then ()
  else
    burn (l @ l |> List.map (fun x -> x + 1))

let run w a =
  match w with
  | Burn i -> burn [i]
  | Tak i ->
    for _ = 1 to i do
      assert (7 = tak 18 12 6);
    done
  | Atomic_incr -> Atomic.incr a
  | Gc_minor -> Gc.minor ()
