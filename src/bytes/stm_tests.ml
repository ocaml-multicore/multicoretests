open QCheck
open STM

(** parallel STM tests of Bytes *)

module ByConf =
struct
  type cmd =
    | Length
    | Get of int
    | Set of int * char
    | Copy
    | To_string
    | Sub of int * int
    | Sub_string of int * int
    | Fill of int * int * char
    | Blit_string of string * int * int * int
    | Index of char
    | Index_opt of char
    | To_seq

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Length -> cst0 "Length" fmt
    | Get x -> cst1 pp_int "Get" par fmt x
    | Set (x, y) -> cst2 pp_int pp_char "Set" par fmt x y
    | Copy -> cst0 "Copy" fmt
    | To_string -> cst0 "To_string" fmt
    | Sub (x, y) -> cst2 pp_int pp_int "Sub" par fmt x y
    | Sub_string (x, y) -> cst2 pp_int pp_int "Sub_string" par fmt x y
    | Fill (x, y, z) -> cst3 pp_int pp_int pp_char "Fill" par fmt x y z
    | Blit_string (x, y, z, w) -> cst4 pp_string pp_int pp_int pp_int "Blit_string" par fmt x y z w
    | Index x -> cst1 pp_char "Index" par fmt x
    | Index_opt x -> cst1 pp_char "Index_opt" par fmt x
    | To_seq -> cst0 "To_seq" fmt

  let show_cmd = Util.Pp.to_show pp_cmd

  type state = char list
  type sut   = Bytes.t

  let shrink_char c = if c = 'a' then Iter.empty else Iter.return 'a' (* much faster than the default *)

  let shrink_cmd c = match c with
    | Blit_string (src,spos,dpos,l) ->
      let open Iter in (* shrink spos int before src string *)
      (Iter.map (fun spos -> Blit_string (src,spos,dpos,l)) (Shrink.int spos))
      <+> (Iter.map (fun src -> Blit_string (src,spos,dpos,l)) (Shrink.string ~shrink:shrink_char src))
    | _ -> Iter.empty

  let arb_cmd s =
    let int_gen = Gen.(oneof [small_nat; int_bound (List.length s - 1)]) in
    let char_gen = Gen.printable in
    QCheck.make ~print:show_cmd ~shrink:shrink_cmd
      Gen.(oneof
             [ return Length;
               map (fun i -> Get i) int_gen;
               map2 (fun i c -> Set (i,c)) int_gen char_gen;
               return Copy;
               return To_string;
               map2 (fun i len -> Sub (i,len)) int_gen int_gen; (* hack: reusing int_gen for length *)
               map2 (fun i len -> Sub_string (i,len)) int_gen int_gen; (* hack: reusing int_gen for length *)
               map3 (fun i len c -> Fill (i,len,c)) int_gen int_gen char_gen; (* hack: reusing int_gen for length*)
               map4 (fun src spos dpos l -> Blit_string (src,spos,dpos,l)) string_small int_gen int_gen int_gen; (* hack: reusing int_gen for length*)
               map (fun c -> Index c) char_gen;
               map (fun c -> Index_opt c) char_gen;
               return To_seq;
             ])

  let byte_size = 16

  let init_state  = List.init byte_size (fun _ -> 'a')

  let next_state c s = match c with
    | Length -> s
    | Get _  -> s
    | Set (i,c) -> List.mapi (fun j c' -> if i = j then c else c') s
    | Copy -> s
    | To_string -> s
    | Sub (_,_) -> s
    | Sub_string (_,_) -> s
    | Fill (i,l,c) ->
      if i >= 0 && l >= 0 && i+l-1 < (List.length s)
        then List.mapi (fun j c' -> if i <= j && j <= i+l-1 then c else c') s
        else s
    | Blit_string (src,spos,dpos,l) ->
      if spos >= 0 && l >= 0 && spos+l-1 < (String.length src)
         && dpos >= 0 && dpos+l-1 < (List.length s)
        then List.mapi (fun j c' -> if dpos <= j && j <= dpos+l-1 then src.[spos+j-dpos] else c') s
        else s
    | Index _ -> s
    | Index_opt _ -> s
    | To_seq -> s

  let init_sut () = Bytes.make byte_size 'a'
  let cleanup _   = ()

  let precond c _s = match c with
    | _ -> true

  let run c b = match c with
    | Length       -> Res (int, Bytes.length b)
    | Get i        -> Res (result char exn, protect (Bytes.get b) i)
    | Set (i,c)    -> Res (result unit exn, protect (Bytes.set b i) c)
    | Copy         -> Res (bytes, Bytes.copy b)
    | To_string    -> Res (string, Bytes.to_string b)
    | Sub (i,l)    -> Res (result bytes exn, protect (Bytes.sub b i) l)
    | Sub_string (i,l) -> Res (result string exn, protect (Bytes.sub_string b i) l)
    | Fill (i,l,c) -> Res (result unit exn, protect (Bytes.fill b i l) c)
    | Blit_string (src,spos,dpos,l) -> Res (result unit exn, protect (Bytes.blit_string src spos b dpos) l)
    | Index c      -> Res (result int exn, protect (Bytes.index b) c)
    | Index_opt c  -> Res (option int, Bytes.index_opt b c)
    | To_seq       -> Res (seq char, List.to_seq (List.of_seq (Bytes.to_seq b)))

  let postcond c (s: char list) res = match c, res with
    | Length, Res ((Int,_),i) -> i = List.length s
    | Get i, Res ((Result (Char,Exn),_), r) ->
      if i < 0 || i >= List.length s
        then r = Error (Invalid_argument "index out of bounds")
        else r = Ok (List.nth s i)
    | Set (i,_), Res ((Result (Unit,Exn),_), r) ->
      if i < 0 || i >= List.length s
        then r = Error (Invalid_argument "index out of bounds")
        else r = Ok ()
    | Copy, Res ((Bytes,_),r) -> r = Bytes.of_seq (List.to_seq s)
    | To_string, Res ((String,_),r) -> r = String.of_seq (List.to_seq s)
    | Sub (i,l), Res ((Result (Bytes,Exn),_), r) ->
      if i < 0 || l < 0 || i+l > List.length s
        then r = Error (Invalid_argument "String.sub / Bytes.sub")
        else r = Ok (Bytes.of_seq (List.to_seq (List.filteri (fun j _ -> i <= j && j <= i+l-1) s)))
    | Sub_string (i,l), Res ((Result (String,Exn),_), r) ->
      if i < 0 || l < 0 || i+l > List.length s
        then r = Error (Invalid_argument "String.sub / Bytes.sub")
        else r = Ok (String.of_seq (List.to_seq (List.filteri (fun j _ -> i <= j && j <= i+l-1) s)))
    | Fill (i,l,_), Res ((Result (Unit,Exn),_), r) ->
      if i < 0 || l < 0 || i+l > List.length s
        then r = Error (Invalid_argument "String.fill / Bytes.fill")
        else r = Ok ()
    | Blit_string (src,spos,dpos,l), Res ((Result (Unit,Exn),_), r) ->
      if spos < 0 || dpos < 0 || l < 0 || spos+l > String.length src || dpos+l > List.length s
        then r = Error (Invalid_argument "String.blit / Bytes.blit_string")
        else r = Ok ()
    | Index c, Res ((Result (Int,Exn),_), r) ->
      (match List.find_index (fun c' -> c' = c) s with
       | Some i -> r = Ok i
       | None -> r = Error Not_found)
    | Index_opt c, Res ((Option Int,_), r) ->
      r = List.find_index (fun c' -> c' = c) s
    | To_seq, Res ((Seq Char,_),r) -> Seq.equal (=) r (List.to_seq s)
    | _, _ -> false
end

module BytesSTM_seq = STM_sequential.Make(ByConf)
module BytesSTM_dom = STM_domain.Make(ByConf)
;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [BytesSTM_seq.agree_test     ~count ~name:"STM Bytes test sequential";
    BytesSTM_dom.neg_agree_test_par ~count ~name:"STM Bytes test parallel"
])
