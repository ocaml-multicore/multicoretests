open QCheck

(* ********************************************************************** *)
(*                      Tests of thread-unsafe [Array]                    *)
(* ********************************************************************** *)
module AConf =
struct
  type t = char array

  type cmd =
    | Length
    | Get of int
    | Set of int * char
    | Sub of int * int
    | Copy
    | Fill of int * int * char
    | To_list
    | Mem of char
    | Sort
    | To_seq

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Length -> cst0 "Length" fmt
    | Get x -> cst1 pp_int "Get" par fmt x
    | Set (x, y) -> cst2 pp_int pp_char "Set" par fmt x y
    | Sub (x, y) -> cst2 pp_int pp_int "Sub" par fmt x y
    | Copy -> cst0 "Copy" fmt
    | Fill (x, y, z) -> cst3 pp_int pp_int pp_char "Fill" par fmt x y z
    | To_list -> cst0 "To_list" fmt
    | Mem x -> cst1 pp_char "Mem" par fmt x
    | Sort -> cst0 "Sort" fmt
    | To_seq -> cst0 "To_seq" fmt

  let show_cmd = Util.Pp.to_show pp_cmd

  let gen_cmd =
    let open QCheck.Gen in
    let int = small_nat and char = printable in
    oneof
      [
        pure Length;
        map (fun x -> Get x) int;
        map2 (fun x y -> Set (x, y)) int char;
        map2 (fun x y -> Sub (x, y)) int int;
        pure Copy;
        map3 (fun x y z -> Fill (x, y, z)) int int char;
        pure To_list;
        map (fun x -> Mem x) char;
        pure Sort;
        pure To_seq;
      ]

  let shrink_cmd c = Iter.empty

  type res =
    | RLength of int
    | RGet of (char, exn) result
    | RSet of (unit, exn) result
    | RSub of (char array, exn) result
    | RCopy of char array
    | RFill of (unit, exn) result
    | RTo_list of char list
    | RMem of bool
    | RSort of unit
    | RTo_seq of char Seq.t

  let pp_res par fmt x =
    let open Util.Pp in
    match x with
    | RLength x -> cst1 pp_int "RLength" par fmt x
    | RGet x -> cst1 (pp_result pp_char pp_exn) "RGet" par fmt x
    | RSet x -> cst1 (pp_result pp_unit pp_exn) "RSet" par fmt x
    | RSub x -> cst1 (pp_result (pp_array pp_char) pp_exn) "RSub" par fmt x
    | RCopy x -> cst1 (pp_array pp_char) "RCopy" par fmt x
    | RFill x -> cst1 (pp_result pp_unit pp_exn) "RFill" par fmt x
    | RTo_list x -> cst1 (pp_list pp_char) "RTo_list" par fmt x
    | RMem x -> cst1 pp_bool "RMem" par fmt x
    | RSort x -> cst1 pp_unit "RSort" par fmt x
    | RTo_seq x -> cst1 (pp_seq pp_char) "RTo_seq" par fmt x

  let show_res = Util.Pp.to_show pp_res

  let equal_res x y =
    let open Util.Equal in
    match (x, y) with
    | RLength x, RLength y -> equal_int x y
    | RGet x, RGet y -> equal_result equal_char equal_exn x y
    | RSet x, RSet y -> equal_result equal_unit equal_exn x y
    | RSub x, RSub y -> equal_result (equal_array equal_char) equal_exn x y
    | RCopy x, RCopy y -> equal_array equal_char x y
    | RFill x, RFill y -> equal_result equal_unit equal_exn x y
    | RTo_list x, RTo_list y -> equal_list equal_char x y
    | RMem x, RMem y -> equal_bool x y
    | RSort x, RSort y -> equal_unit x y
    | RTo_seq x, RTo_seq y -> equal_seq equal_char x y
    | _, _ -> false

  let array_size = 16

  let init () = Array.make array_size 'a'

  let run c a = match c with
    | Length        -> RLength (Array.length a)
    | Get i         -> RGet (Util.protect (Array.get a) i)
    | Set (i,c)     -> RSet (Util.protect (Array.set a i) c)
    | Sub (i,l)     -> RSub (Util.protect (Array.sub a i) l)
    | Copy          -> RCopy (Array.copy a)
    | Fill (i,l,c)  -> RFill (Util.protect (Array.fill a i l) c)
    | To_list       -> RTo_list (Array.to_list a)
    | Mem c         -> RMem (Array.mem c a)
    | Sort          -> RSort (Array.sort Char.compare a)
    | To_seq        -> RTo_seq (List.to_seq (List.of_seq (Array.to_seq a))) (* workaround: Array.to_seq is lazy and will otherwise see and report later Array.set state changes... *)
  let cleanup _ = ()
end

module AT_domain = Lin_domain.Make_internal(AConf) [@alert "-internal"]
;;
QCheck_base_runner.run_tests_main [
  AT_domain.neg_lin_test ~count:1000 ~name:"Lin.Internal Array test with Domain";
]
