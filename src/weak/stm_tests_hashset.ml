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
  and data = string
  [@@deriving show { with_path = false }]

  type state = data list

  module WHS = Weak.Make(String)
  type sut = WHS.t

  let shrink_string s = Shrink.string ~shrink:Shrink.nil s

  let shrink_cmd c = match c with
    | Clear -> Iter.empty
    | Merge d -> Iter.map (fun d -> Merge d) (shrink_string d)
    | Add d -> Iter.map (fun d -> Add d) (shrink_string d)
    | Remove d -> Iter.map (fun d -> Remove d) (shrink_string d)
    | Find d -> Iter.map (fun d -> Find d) (shrink_string d)
    | Find_opt d -> Iter.map (fun d -> Find_opt d) (shrink_string d)
    | Find_all d -> Iter.map (fun d -> Find_all d) (shrink_string d)
    | Mem d -> Iter.map (fun d -> Mem d) (shrink_string d)
    | Count -> Iter.empty
    | Stats -> Iter.empty

  let arb_cmd s =
    let data_gen = match s with
      | [] -> Gen.string_small
      | _::_ -> Gen.(oneof [oneofl s; string_small]) in
    QCheck.make ~print:show_cmd ~shrink:shrink_cmd
      Gen.(oneof
             [ return Clear;
               map (fun d -> Merge d) data_gen;
               map (fun d -> Add d) data_gen;
               map (fun d -> Remove d) data_gen;
               map (fun d -> Find d) data_gen;
               map (fun d -> Find_opt d) data_gen;
               map (fun d -> Find_all d) data_gen;
               map (fun d -> Mem d) data_gen;
               return Count;
               return Stats;
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
  let init_sut () = WHS.create weak_size
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
    | Merge d    -> Res (result string exn, protect (WHS.merge hs) d)
    | Add d      -> Res (result unit exn, protect (WHS.add hs) d)
    | Remove d   -> Res (result unit exn, protect (WHS.remove hs) d)
    | Find d     -> Res (result string exn, protect (WHS.find hs) d)
    | Find_opt d -> Res (result (option string) exn, protect (WHS.find_opt hs) d)
    | Find_all d -> Res (result (list string) exn, protect (WHS.find_all hs) d)
    | Mem d      -> Res (result bool exn, protect (WHS.mem hs) d)
    | Count      -> Res (int, WHS.count hs)
    | Stats      -> Res (tup6 int int int int int int, WHS.stats hs)

  let postcond c (s:data list) res = match c, res with
    | Clear, Res ((Unit,_),())  -> true
    | Merge d, Res ((Result (String,Exn),_),r) ->
      (match r with
       | Error e -> e = Invalid_argument "index out of bounds"
       | Ok r -> if List.mem d s then r = d else r == d)
    | Add _, Res ((Result (Unit,Exn),_),r) ->
      r = Error (Invalid_argument "index out of bounds") || r = Ok ()
    | Remove _, Res ((Result (Unit,Exn),_),r) ->
      r = Error (Invalid_argument "index out of bounds") || r = Ok ()
    | Find d, Res ((Result (String,Exn),_),r) ->
      r = Error (Invalid_argument "index out of bounds") ||
      r = Error Not_found ||
      (List.mem d s && r = Ok d)
    | Find_opt d, Res ((Result (Option String,Exn),_),r) ->
      r = Error (Invalid_argument "index out of bounds") ||
      r = Ok None || r = Ok (Some d)
    | Find_all d, Res ((Result (List String,Exn),_),r) ->
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
(*module WeakHashsetSTM_dom = STM_domain.Make(WHSConf)*)
;;
QCheck_base_runner.run_tests_main
  [ WeakHashsetSTM_seq.agree_test         ~count:1000 ~name:"STM Weak HashSet test sequential";
    (*WeakHashsetSTM_dom.neg_agree_test_par ~count:1000 ~name:"STM Weak HashSet test parallel";*)
  ]
