open QCheck
open STM

(** STM specification of Weak hashsets *)

type cmd =
  | Clear
  | Merge of data
  | Add of data
  | Remove of data
  | Mem of data
  (*val iter : (data -> unit) -> t -> unit*)
  (*val fold : (data -> 'a -> 'a) -> t -> 'a -> 'a*)
  | Find of data
  | Find_opt of data
  | Find_all of data
  | Count
  | Stats
and data = string

let pp_cmd par fmt x =
  let open Util.Pp in
  let pp_data = pp_string in
  match x with
  | Clear -> cst0 "Clear" fmt
  | Merge x -> cst1 pp_data "Merge" par fmt x
  | Add x -> cst1 pp_data "Add" par fmt x
  | Remove x -> cst1 pp_data "Remove" par fmt x
  | Mem x -> cst1 pp_data "Mem" par fmt x
  | Find x -> cst1 pp_data "Find" par fmt x
  | Find_opt x -> cst1 pp_data "Find_opt" par fmt x
  | Find_all x -> cst1 pp_data "Find_all" par fmt x
  | Count -> cst0 "Count" fmt
  | Stats -> cst0 "Stats" fmt

let show_cmd = Util.Pp.to_show pp_cmd

type state = data list

module WHS = Weak.Make(String)
type sut = WHS.t

let shrink_data d = Shrink.string ~shrink:Shrink.nil d

let _shrink_cmd c = match c with
  | Clear -> Iter.empty
  | Merge d -> Iter.map (fun d -> Merge d) (shrink_data d)
  | Add d -> Iter.map (fun d -> Add d) (shrink_data d)
  | Remove d -> Iter.map (fun d -> Remove d) (shrink_data d)
  | Find d -> Iter.map (fun d -> Find d) (shrink_data d)
  | Find_opt d -> Iter.map (fun d -> Find_opt d) (shrink_data d)
  | Find_all d -> Iter.map (fun d -> Find_all d) (shrink_data d)
  | Mem d -> Iter.map (fun d -> Mem d) (shrink_data d)
  | Count -> Iter.empty
  | Stats -> Iter.empty

let arb_cmd s =
  let string_gen = Gen.(string_small_of printable) in
  let data_gen = match s with
    | [] -> string_gen
    | _::_ -> Gen.(oneof [oneofl s; string_gen]) in
  QCheck.make ~print:show_cmd (*~shrink:shrink_cmd*)
    Gen.(frequency
           [ 1,return Clear;
             2,map (fun d -> Merge d) data_gen;
             2,map (fun d -> Add d) data_gen;
             1,map (fun d -> Remove d) data_gen;
             1,map (fun d -> Find d) data_gen;
             1,map (fun d -> Find_opt d) data_gen;
             1,map (fun d -> Find_all d) data_gen;
             1,map (fun d -> Mem d) data_gen;
             3,return Count;
             2,return Stats;
           ])

let init_state = []

let rec remove_first d s = match s with
  | [] -> s
  | d'::s' -> if d=d' then s' else d'::(remove_first d s')

let next_state c s = match c with
  | Clear    -> []
  | Merge d  -> if List.mem d s then s else d::s
  | Add d    -> d::s
  | Remove d -> remove_first d s
  | Find _
  | Find_opt _
  | Find_all _
  | Mem _
  | Count
  | Stats -> s

let weak_size = 16
(* The SUT state is GC dependent - run a minor GC to have a clean starting point *)
let init_sut () = Gc.minor (); WHS.create weak_size
let cleanup _   = ()

let precond _c _s = true

type _ ty += Tup6 : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty * 'f ty -> ('a * 'b * 'c * 'd * 'e * 'f) ty

let tup6 spec_a spec_b spec_c spec_d spec_e spec_f =
  let (ty_a,show_a) = spec_a in
  let (ty_b,show_b) = spec_b in
  let (ty_c,show_c) = spec_c in
  let (ty_d,show_d) = spec_d in
  let (ty_e,show_e) = spec_e in
  let (ty_f,show_f) = spec_f in
  (Tup6 (ty_a,ty_b,ty_c,ty_d,ty_e,ty_f),
   QCheck.Print.tup6 show_a show_b show_c show_d show_e show_f)

let run c hs = match c with
  | Clear      -> Res (unit, WHS.clear hs)
  | Merge d    -> Res (result string exn, protect (fun () -> WHS.merge hs d) ())
  | Add d      -> Res (result unit exn, protect (fun () -> WHS.add hs d) ())
  | Remove d   -> Res (result unit exn, protect (fun () -> WHS.remove hs d) ())
  | Find d     -> Res (result string exn, protect (fun () -> WHS.find hs d) ())
  | Find_opt d -> Res (result (option string) exn, protect (fun () -> WHS.find_opt hs d) ())
  | Find_all d -> Res (result (list string) exn, protect (fun () -> WHS.find_all hs d) ())
  | Mem d      -> Res (result bool exn, protect (fun () -> WHS.mem hs d) ())
  | Count      -> Res (int, WHS.count hs)
  | Stats      -> Res (tup6 int int int int int int, WHS.stats hs)

let postcond c (s:data list) res = match c, res with
  | Clear, Res ((Unit,_),())  -> true
  | Merge d, Res ((Result (String,Exn),_),r) ->
    (match r with
     | Error _ -> false
     | Ok r -> if List.mem d s then r = d else r == d)
  | Add _, Res ((Result (Unit,Exn),_),r) ->
    r = Ok ()
  | Remove _, Res ((Result (Unit,Exn),_),r) ->
    r = Ok ()
  | Find d, Res ((Result (String,Exn),_),r) ->
    r = Error Not_found || (List.mem d s && r = Ok d)
  | Find_opt d, Res ((Result (Option String,Exn),_),r) ->
    r = Ok None || (List.mem d s && r = Ok (Some d))
  | Find_all d, Res ((Result (List String,Exn),_),r) ->
    (match r with
     | Error _ -> false
     | Ok r -> List.for_all (fun d' -> d' = d) r)
  | Mem d, Res ((Result (Bool,Exn),_),r) ->
    r = Ok (List.mem d s) || r = Ok false
  | Count, Res ((Int,_),r) ->
    r <= List.length s
  | Stats, Res ((Tup6 (Int,Int,Int,Int,Int,Int),_),r) ->
    let (len,entries,sum,smallest,median,biggest) = r in
    len = weak_size && entries <= List.length s &&
    sum >= 0 && smallest >= 0 && median >= 0 && biggest >= 0
  | _,_ -> false
