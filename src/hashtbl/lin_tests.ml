open QCheck
open Lin_base.Lin_internal

(** ********************************************************************** *)
(**                      Tests of thread-unsafe [Hashtbl]                  *)
(** ********************************************************************** *)
module HConf =
struct
  type t = (char, int) Hashtbl.t

  type cmd =
    | Clear
    | Add of char' * int'
    | Remove of char'
    | Find of char'
    | Find_opt of char'
    | Find_all of char'
    | Replace of char' * int'
    | Mem of char'
    | Length [@@deriving qcheck, show { with_path = false }]
  and int' = int [@gen Gen.nat]
  and char' = char [@gen Gen.printable]

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
    | RFind of ((int, exn) result [@equal (=)])
    | RFind_opt of int option
    | RFind_all of int list
    | RReplace
    | RMem of bool
    | RLength of int [@@deriving show { with_path = false }, eq]

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

module HT_domain = Lin_domain.Make_internal(HConf)
;;
QCheck_base_runner.run_tests_main [
  HT_domain.neg_lin_test ~count:1000 ~name:"Lin Hashtbl test with Domain";
]
