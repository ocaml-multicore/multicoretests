open QCheck
open STM

(** STM specification of Weak arrays *)

type cmd =
  | Length
  | Set of int * data option
  | Get of int
  | Get_copy of int
  | Check of int
  | Fill of int * int * data option
and data = string

let pp_cmd par fmt x =
  let open Util.Pp in
  match x with
  | Length -> cst0 "Length" fmt
  | Set (x, y) -> cst2 pp_int (pp_option pp_string) "Set" par fmt x y
  | Get x -> cst1 pp_int "Get" par fmt x
  | Get_copy x -> cst1 pp_int "Get_copy" par fmt x
  | Check x -> cst1 pp_int "Check" par fmt x
  | Fill (x, y, z) ->
    cst3 pp_int pp_int (pp_option pp_string) "Fill" par fmt x y z

let show_cmd = Util.Pp.to_show pp_cmd

type state = data option list
type sut = data Weak.t

let _shrink_cmd c = match c with
  | Length -> Iter.empty
  | Set (i, d_opt) -> Iter.map (fun i -> Set (i,d_opt)) (Shrink.int i)
  | Get i      -> Iter.map (fun i -> Get i) (Shrink.int i)
  | Get_copy i -> Iter.map (fun i -> Get_copy i) (Shrink.int i)
  | Check i    -> Iter.map (fun i -> Check i) (Shrink.int i)
  | Fill (i,j,d_opt) ->
    Iter.(map (fun i -> Fill (i,j,d_opt)) (Shrink.int i)
          <+>
          map (fun j -> Fill (i,j,d_opt)) (Shrink.int j))

let arb_cmd s =
  let int_gen = Gen.(oneof [small_nat; int_bound (List.length s - 1)]) in
  let data_gen = Gen.(string_small_of printable) in
  QCheck.make ~print:show_cmd (*~shrink:shrink_cmd*)
    Gen.(frequency
           [ 1,return Length;
             1,map2 (fun i d_opt -> Set (i,d_opt)) int_gen (option data_gen);
             2,map (fun i -> Get i) int_gen;
             2,map (fun i -> Get_copy i) int_gen;
             2,map (fun i -> Check i) int_gen;
             2,map3 (fun i len d_opt -> Fill (i,len,d_opt)) int_gen int_gen (option data_gen); (* hack: reusing int_gen for length *)
           ])

let weak_size = 10

let init_state  = List.init weak_size (fun _ -> None)

let next_state c s = match c with
  | Length -> s
  | Set (i,d_opt) ->
    List.mapi (fun j d_opt' -> if i=j then d_opt else d_opt') s
  | Get _ -> s
  | Get_copy _ -> s
  | Check _  -> s
  | Fill (i,l,d_opt) ->
    if i >= 0 && l >= 0 && i+l-1 < List.length s
    then
      List.mapi (fun j d_opt' -> if i <= j && j <= i+l-1 then d_opt else d_opt') s
    else s

let init_sut () = Gc.minor (); Weak.create weak_size
let cleanup _   = ()

let precond _c _s = true

let run c a = match c with
  | Length       -> Res (int, Weak.length a)
  | Set (i,d_opt)    -> Res (result unit exn, protect (fun () -> Weak.set a i d_opt) ())
  | Get i        -> Res (result (option string) exn, protect (fun () -> Weak.get a i) ())
  | Get_copy i   -> Res (result (option string) exn, protect (fun () -> Weak.get_copy a i) ())
  | Check i      -> Res (result bool exn, protect (fun () -> Weak.check a i) ())
  | Fill (i,l,d_opt) -> Res (result unit exn, protect (fun () -> Weak.fill a i l d_opt) ())

let postcond c (s:state) res = match c, res with
  | Length, Res ((Int,_),i) -> i = List.length s
  | Set (i,_), Res ((Result (Unit,Exn),_), r) ->
    if i < 0 || i >= List.length s
    then r = Error (Invalid_argument "Weak.set")
    else r = Ok ()
  | Get i, Res ((Result (Option String,Exn),_), r) ->
    if i < 0 || i >= List.length s
    then r = Error (Invalid_argument "Weak.get")
    else r = Ok None || r = Ok (List.nth s i)
  | Get_copy i, Res ((Result (Option String,Exn),_), r) ->
    if i < 0 || i >= List.length s
    then r = Error (Invalid_argument "Weak.get_copy")
    else r = Ok None || r = Ok (List.nth s i)
  | Check i, Res ((Result (Bool,Exn),_),r) ->
    if i < 0 || i >= List.length s
    then r = Error (Invalid_argument "Weak.check")
    else r = Ok false || r = Ok (None <> List.nth s i)
  | Fill (i,l,_), Res ((Result (Unit,Exn),_), r) ->
    if i < 0 || l < 0 || i+l > List.length s
    then r = Error (Invalid_argument "Weak.fill")
    else r = Ok ()
  | _, _ -> false

