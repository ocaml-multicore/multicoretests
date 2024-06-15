open QCheck
open STM

(** parallel STM tests of Weak hashsets *)

module WHSConf =
struct
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
  and data = int64

  let pp_cmd par fmt x =
    let open Util.Pp in
    let pp_data = pp_int64 in
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

  module Int64 =
  struct
    [@@@warning "-unused-value-declaration"]
    (* support Int64.hash added in 5.1, without triggering an 'unused hash' error *)
    external seeded_hash_param :
      int -> int -> int -> 'a -> int = "caml_hash" [@@noalloc]
    let hash x = seeded_hash_param 10 100 0 x
    include Stdlib.Int64
  end
  module WHS = Weak.Make(Int64)
  type sut = WHS.t

  let shrink_data d = Shrink.int64 d

  let shrink_cmd c = match c with
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
    let data_gen = match s with
      | [] -> Gen.(map Int64.of_int small_int)
      | _::_ -> Gen.(oneof [oneofl s; map Int64.of_int small_int]) in
    QCheck.make ~print:show_cmd ~shrink:shrink_cmd
      Gen.(frequency
             [ 1,return Clear;
               1,map (fun d -> Merge d) data_gen;
               1,map (fun d -> Add d) data_gen;
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

  let precond c _s = match c with
    | _ -> true

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
    | Merge d    -> Res (result int64 exn, protect (WHS.merge hs) d)
    | Add d      -> Res (result unit exn, protect (WHS.add hs) d)
    | Remove d   -> Res (result unit exn, protect (WHS.remove hs) d)
    | Find d     -> Res (result int64 exn, protect (WHS.find hs) d)
    | Find_opt d -> Res (result (option int64) exn, protect (WHS.find_opt hs) d)
    | Find_all d -> Res (result (list int64) exn, protect (WHS.find_all hs) d)
    | Mem d      -> Res (result bool exn, protect (WHS.mem hs) d)
    | Count      -> Res (int, WHS.count hs)
    | Stats      -> Res (tup6 int int int int int int, WHS.stats hs)

  let postcond c (s:data list) res = match c, res with
    | Clear, Res ((Unit,_),())  -> true
    | Merge d, Res ((Result (Int64,Exn),_),r) ->
      (match r with
       | Error e -> e = Invalid_argument "index out of bounds"
       | Ok r -> if List.mem d s then r = d else r == d)
    | Add _, Res ((Result (Unit,Exn),_),r) ->
      r = Error (Invalid_argument "index out of bounds") || r = Ok ()
    | Remove _, Res ((Result (Unit,Exn),_),r) ->
      r = Error (Invalid_argument "index out of bounds") || r = Ok ()
    | Find d, Res ((Result (Int64,Exn),_),r) ->
      r = Error (Invalid_argument "index out of bounds") ||
      r = Error Not_found ||
      (List.mem d s && r = Ok d)
    | Find_opt d, Res ((Result (Option Int64,Exn),_),r) ->
      r = Error (Invalid_argument "index out of bounds") ||
      r = Ok None || r = Ok (Some d)
    | Find_all d, Res ((Result (List Int64,Exn),_),r) ->
      (match r with
       | Error e -> e = Invalid_argument "index out of bounds"
       | Ok r -> List.for_all (fun d' -> d' = d) r)
    | Mem d, Res ((Result (Bool,Exn),_),r) ->
      r = Error (Invalid_argument "index out of bounds") ||
      r = Ok (List.mem d s) || r = Ok false
    | Count, Res ((Int,_),r) ->
      r <= List.length s
    | Stats, Res ((Tup6 (Int,Int,Int,Int,Int,Int),_),r) ->
      let (len,entries,sum,smallest,median,biggest) = r in
      len = weak_size && entries <= List.length s &&
      sum >= 0 && smallest >= 0 && median >= 0 && biggest >= 0
    | _,_ -> false
end

module WeakHashsetSTM_seq = STM_sequential.Make(WHSConf)
module WeakHashsetSTM_dom = STM_domain.Make(WHSConf)

(* Beware: hoop jumping to enable a full major Gc run between the two tests!
   We need that to avoid the state of the second test depending on the resulting
   GC state of the first test and don't want to exit after the first run
   (as QCheck_base_runner.run_tests_main does). *)
let cli_args = QCheck_base_runner.Raw.parse_cli ~full_options:false Sys.argv
let run_tests l =
  QCheck_base_runner.run_tests l
    ~colors:cli_args.cli_colors
    ~verbose:cli_args.cli_verbose
    ~long:cli_args.cli_long_tests ~out:stdout ~rand:cli_args.cli_rand
let status_seq =
  run_tests
    [ WeakHashsetSTM_seq.agree_test         ~count:1000 ~name:"STM Weak HashSet test sequential" ]
let () = Gc.full_major ()
let status_par =
  run_tests
    [ WeakHashsetSTM_dom.neg_agree_test_par ~count:5000 ~name:"STM Weak HashSet test parallel" ]
let () = Gc.full_major ()
let status_stress =
  run_tests
    [ WeakHashsetSTM_dom.stress_test_par    ~count:1000 ~name:"STM Weak HashSet stress test parallel"; ]
let _ = exit (if status_seq=0 && status_par=0 && status_stress=0 then 0 else 1)
