open QCheck
open Lin

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

module HT = Lin.Make(HConf)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main [
  HT.lin_test     `Domain ~count:1000 ~name:"Hashtbl test with domains";
]
