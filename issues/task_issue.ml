open Domainslib

(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
           else z

let rec repeat n =
  if n <= 0 then 0 else tak 18 12 6 + repeat(n-1)

let pool = Task.setup_pool ~num_additional_domains:3 ()

let p0 = Task.async pool (fun () -> repeat 200)
let p1 = Task.async pool (fun () -> let r = repeat 200 in
                                    let other = Task.await pool p0 in
                                    assert (r=other);
                                    r)
let p2 = Task.async pool (fun () -> repeat 200)
let p3 = Task.async pool (fun () -> let r = repeat 200 in
                                    let other = Task.await pool p1 in
                                    assert (r=other);
                                    r)
let p4 = Task.async pool (fun () -> repeat 200)
let p5 = Task.async pool (fun () -> repeat 200)

let _ = List.fold_left (fun acc p -> assert (acc = Task.await pool p); acc) 1400 [p0;p1;p2;p3;p4;p5]
let () = Task.teardown_pool pool
