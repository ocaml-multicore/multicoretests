open QCheck

(* ********************************************************************** *)
(*                      Tests of thread-unsafe [Hashtbl]                  *)
(* ********************************************************************** *)
module HConf =
struct
  type t = (char, int) Hashtbl.t

  type cmd =
    | Clear
    | Add of char * int
    | Remove of char
    | Find of char
    | Find_opt of char
    | Find_all of char
    | Replace of char * int
    | Mem of char
    | Length

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Clear -> cst0 "Clear" fmt
    | Add (x, y) -> cst2 pp_char pp_int "Add" par fmt x y
    | Remove x -> cst1 pp_char "Remove" par fmt x
    | Find x -> cst1 pp_char "Find" par fmt x
    | Find_opt x -> cst1 pp_char "Find_opt" par fmt x
    | Find_all x -> cst1 pp_char "Find_all" par fmt x
    | Replace (x, y) -> cst2 pp_char pp_int "Replace" par fmt x y
    | Mem x -> cst1 pp_char "Mem" par fmt x
    | Length -> cst0 "Length" fmt

  let show_cmd = Util.Pp.to_show pp_cmd

  let gen_cmd =
    let open QCheck.Gen in
    let int = nat and char = printable in
    oneof
      [
        pure Clear;
        map2 (fun x y -> Add (x, y)) char int;
        map (fun x -> Remove x) char;
        map (fun x -> Find x) char;
        map (fun x -> Find_opt x) char;
        map (fun x -> Find_all x) char;
        map2 (fun x y -> Replace (x, y)) char int;
        map (fun x -> Mem x) char;
        pure Length;
      ]

  let shrink_cmd c = match c with
    | Clear -> Iter.empty
    | Add (c,i) ->
        Iter.((map (fun c -> Add (c,i)) (Shrink.char c))
              <+>
              (map (fun i -> Add (c,i)) (Shrink.int i)))
    | Remove c -> Iter.map (fun c -> Remove c) (Shrink.char c)
    | Find c -> Iter.map (fun c -> Find c) (Shrink.char c)
    | Find_opt c -> Iter.map (fun c -> Find_opt c) (Shrink.char c)
    | Find_all c -> Iter.map (fun c -> Find_all c) (Shrink.char c)
    | Replace (c,i) ->
        Iter.((map (fun c -> Replace (c,i)) (Shrink.char c))
              <+>
              (map (fun i -> Replace (c,i)) (Shrink.int i)))
    | Mem c -> Iter.map (fun c -> Mem c) (Shrink.char c)
    | Length -> Iter.empty

  type res =
    | RClear
    | RAdd
    | RRemove
    | RFind of (int, exn) result
    | RFind_opt of int option
    | RFind_all of int list
    | RReplace
    | RMem of bool
    | RLength of int

  let pp_res par fmt x =
    let open Util.Pp in
    match x with
    | RClear -> cst0 "RClear" fmt
    | RAdd -> cst0 "RAdd" fmt
    | RRemove -> cst0 "RRemove" fmt
    | RFind x -> cst1 (pp_result pp_int pp_exn) "RFind" par fmt x
    | RFind_opt x -> cst1 (pp_option pp_int) "RFind_opt" par fmt x
    | RFind_all x -> cst1 (pp_list pp_int) "RFind_all" par fmt x
    | RReplace -> cst0 "RReplace" fmt
    | RMem x -> cst1 pp_bool "RMem" par fmt x
    | RLength x -> cst1 pp_int "RLength" par fmt x

  let show_res = Util.Pp.to_show pp_res

  let equal_res x y =
    let open Util.Equal in
    match (x, y) with
    | RClear, RClear -> true
    | RAdd, RAdd -> true
    | RRemove, RRemove -> true
    | RFind x, RFind y -> equal_result equal_int equal_exn x y
    | RFind_opt x, RFind_opt y -> equal_option equal_int x y
    | RFind_all x, RFind_all y -> equal_list equal_int x y
    | RReplace, RReplace -> true
    | RMem x, RMem y -> equal_bool x y
    | RLength x, RLength y -> equal_int x y
    | _, _ -> false

  let init () = Hashtbl.create ~random:false 42

  let run c h = match c with
    | Clear         -> Hashtbl.clear h; RClear
    | Add (k,v)     -> Hashtbl.add h k v; RAdd
    | Remove k      -> Hashtbl.remove h k; RRemove
    | Find k        -> RFind (Util.protect (Hashtbl.find h) k)
    | Find_opt k    -> RFind_opt (Hashtbl.find_opt h k)
    | Find_all k    -> RFind_all (Hashtbl.find_all h k)
    | Replace (k,v) -> Hashtbl.replace h k v; RReplace
    | Mem k         -> RMem (Hashtbl.mem h k)
    | Length        -> RLength (Hashtbl.length h)

  let cleanup _ = ()
end

module HT_domain = Lin_domain.Make_internal(HConf) [@alert "-internal"]
;;
QCheck_base_runner.run_tests_main [
  HT_domain.neg_lin_test ~count:1000 ~name:"Lin.Internal Hashtbl test with Domain";
]
