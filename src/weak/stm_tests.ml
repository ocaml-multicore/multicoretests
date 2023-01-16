open QCheck
open STM

(** parallel STM tests of Weak arrays *)

module WConf =
struct
  type cmd =
    | Length
    | Set of int * int64 option
    | Get of int
    | Get_copy of int
    | Check of int
    | Fill of int * int * int64 option
  [@@deriving show { with_path = false }]

  type state = int64 option list
  type sut = int64 Weak.t

  let arb_cmd s =
    let int_gen = Gen.(oneof [small_nat; int_bound (List.length s - 1)]) in
    let int64_gen = Gen.(map Int64.of_int small_int) in
    QCheck.make ~print:show_cmd (*~shrink:shrink_cmd*)
      Gen.(oneof
             [ return Length;
               map2 (fun i c -> Set (i,c)) int_gen (option int64_gen);
               map (fun i -> Get i) int_gen;
               map (fun i -> Get_copy i) int_gen;
               map (fun i -> Check i) int_gen;
               map3 (fun i len c -> Fill (i,len,c)) int_gen int_gen (option int64_gen); (* hack: reusing int_gen for length *)
             ])

  let weak_size = 16

  let init_state  = List.init weak_size (fun _ -> None)

  let next_state c s = match c with
    | Length -> s
    | Set (i,c) ->
      List.mapi (fun j c' -> if i=j then c else c') s
    | Get _  -> s
    | Get_copy _  -> s
    | Check _  -> s
    | Fill (i,l,c) ->
      if i >= 0 && l >= 0 && i+l-1 < List.length s
      then
        List.mapi (fun j c' -> if i <= j && j <= i+l-1 then c else c') s
      else s

  let init_sut () = Weak.create weak_size
  let cleanup _   = ()

  let precond c _s = match c with
    | _ -> true

  let run c a = match c with
    | Length       -> Res (int, Weak.length a)
    | Set (i,c)    -> Res (result unit exn, protect (Weak.set a i) c)
    | Get i        -> Res (result (option int64) exn, protect (Weak.get a) i)
    | Get_copy i   -> Res (result (option int64) exn, protect (Weak.get_copy a) i)
    | Check i      -> Res (result bool exn, protect (Weak.check a) i)
    | Fill (i,l,c) -> Res (result unit exn, protect (Weak.fill a i l) c)

  let postcond c (s:int64 option list) res = match c, res with
    | Length, Res ((Int,_),i) -> i = List.length s
    | Set (i,_), Res ((Result (Unit,Exn),_), r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "Weak.set")
      else r = Ok ()
    | Get i, Res ((Result (Option Int64,Exn),_), r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "Weak.get")
      else r = Ok (List.nth s i)
    | Get_copy i, Res ((Result (Option Int64,Exn),_), r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "Weak.get_copy")
      else r = Ok (List.nth s i)
    | Check i, Res ((Result (Bool,Exn),_),r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "Weak.check")
      else r = Ok (None <> List.nth s i)
    | Fill (i,l,_), Res ((Result (Unit,Exn),_), r) ->
      if i < 0 || l < 0 || i+l > List.length s
      then r = Error (Invalid_argument "Weak.fill")
      else r = Ok ()
    | _, _ -> false
end

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

module WeakSTM_seq = STM_sequential.Make(WConf)
module WeakSTM_dom = STM_domain.Make(WConf)

module WeakHashsetSTM_seq = STM_sequential.Make(WHSConf)
module WeakHashsetSTM_dom = STM_domain.Make(WHSConf)

;;
QCheck_base_runner.run_tests_main
  [ WeakSTM_seq.agree_test                ~count:1000 ~name:"STM Weak test sequential";
    WeakSTM_dom.neg_agree_test_par        ~count:1000 ~name:"STM Weak test parallel";
    WeakHashsetSTM_seq.agree_test         ~count:1000 ~name:"STM Weak HashSet test sequential";
    WeakHashsetSTM_dom.neg_agree_test_par ~count:1000 ~name:"STM Weak HashSet test parallel";
  ]
