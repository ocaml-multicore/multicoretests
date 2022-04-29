(*open QCheck*)

(** parallel linearization tests of Lazy *)

(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
           else z

let work () =
  let r = ref 0 in
  for _ = 1 to 100 do
    r := !r + tak 18 12 6;
  done;
  !r

module Spec =
struct
  type cmd =
    | Force_val
    | Is_val
    | Map of int_fun
  and int_fun = (int -> int)

  type res =
    | RForce_val of int
    | RIs_val of bool
    | RMap of int [@@deriving eq]

  let run c l = match c with
    | Force_val -> RForce_val (Lazy.force_val l)
    | Is_val    -> RIs_val (Lazy.is_val l)
    | Map f     -> RMap (Lazy.force (Lazy.map f l)) (*we force the "new lazy"*)

  let init () = lazy (work ())
end

let interp sut cs =
  let cs_arr = Array.of_list cs in
  let res_arr = Array.map (fun c -> Domain.cpu_relax(); Spec.run c sut) cs_arr in
  List.combine cs (Array.to_list res_arr)

let rec check_seq_cons pref cs1 cs2 seq_sut seq_trace = match pref with
  | (c,res)::pref' ->
     if Spec.equal_res res (Spec.run c seq_sut)
     then check_seq_cons pref' cs1 cs2 seq_sut (c::seq_trace)
     else false
  | [] -> match cs1,cs2 with
          | [],[] -> true
          | [],(c2,res2)::cs2' ->
             Spec.equal_res res2 (Spec.run c2 seq_sut) && check_seq_cons pref cs1 cs2' seq_sut (c2::seq_trace)
          | (c1,res1)::cs1',[] ->
             Spec.equal_res res1 (Spec.run c1 seq_sut) && check_seq_cons pref cs1' cs2 seq_sut (c1::seq_trace)
          | (c1,res1)::cs1',(c2,res2)::cs2' ->
             (Spec.equal_res res1 (Spec.run c1 seq_sut) && check_seq_cons pref cs1' cs2 seq_sut (c1::seq_trace))
             ||
               (* rerun to get seq_sut to same cmd branching point *)
               (let seq_sut' = Spec.init () in
                let _ = interp seq_sut' (List.rev seq_trace) in
                Spec.equal_res res2 (Spec.run c2 seq_sut') && check_seq_cons pref cs1 cs2' seq_sut' (c2::seq_trace))

exception NOT_COMPAT
(* Linearizability property based on [Domain] and an Atomic flag *)
let lin_prop_domain (seq_pref,cmds1,cmds2) =
    let sut = Spec.init () in
    let pref_obs = interp sut seq_pref in
    let wait = Atomic.make true in
    let dom1 = Domain.spawn (fun () -> while Atomic.get wait do Domain.cpu_relax() done; interp sut cmds1) in
    let dom2 = Domain.spawn (fun () -> Atomic.set wait false; interp sut cmds2) in
    let obs1 = Domain.join dom1 in
    let obs2 = Domain.join dom2 in
    let seq_sut = Spec.init () in
    check_seq_cons pref_obs obs1 obs2 seq_sut [] || raise NOT_COMPAT

let iter_count = ref 0

let exec_test t =
  try
    Printf.printf "%i %!" !iter_count;
    incr iter_count;
    let b = Util.repeat 50 lin_prop_domain t in
    Printf.printf "%s\n%!" (if b then "t" else "f");
    b
  with CamlinternalLazy.Undefined -> Printf.printf "CamlinternalLazy.Undefined\n%!"; false

let tests =
  let open Spec in
  [
    ([Force_val], [Is_val; (Map (function 700 -> 2 | _ -> 71))], [Force_val; (Map (function 700 -> 9 | _ -> 3))]) ;
    ([Force_val], [Is_val; (Map (function 700 -> 2 | _ -> 71))], [Force_val; (Map (function 700 -> 9 | _ -> 3))]) ;
    ([Force_val], [Is_val; (Map (function 700 -> 2 | _ -> 71))], [Force_val; (Map (function 700 -> 9 | _ -> 3))]) ;
    ([Force_val], [Is_val; (Map (function 700 -> 2 | _ -> 71))], [(Map (function 700 -> 9 | _ -> 3))]) ;
    ([Force_val], [Is_val; (Map (function 700 -> 2 | _ -> 71))], [(Map (function 700 -> 9 | _ -> 3))]) ;
    ([Force_val], [Is_val; (Map (function 700 -> 2 | _ -> 71))], [(Map (function 700 -> 9 | _ -> 3))]) ;
    ([Is_val],    [(Map (function 700 -> 2 | _ -> 71))],         [Force_val; (Map (function 700 -> 9 | _ -> 3))]) ;
    ([Is_val],    [(Map (function 700 -> 2 | _ -> 71))],         [(Map (function 700 -> 9 | _ -> 3))]) ;
    ([(Map (function 700 -> 2 | _ -> 71))], [], [(Map (function 700 -> 9 | _ -> 3))]);
  ]
;;
List.map exec_test tests
