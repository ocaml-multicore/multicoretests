open QCheck
open Util

module Spec =
  struct
    module E = Ephemeron.K1.Make(struct type t = int64 let equal = Int64.equal let hash = Int64.to_int end)
    type t = string E.t
    
    type cmd =
      | Clear
      | Add of int' * string
      | Remove of int'
      | Find of int'
      | Find_opt of int'
      | Find_all of int'
      | Replace of int' * string
      | Mem of int'
      | Length
      | Clean [@@deriving qcheck, show { with_path = false }]
    and int' = int64 [@gen fun st -> Gen.nat st |> Int64.of_int]

    type res =
      | RClear
      | RAdd
      | RRemove
      | RFind of (string, exn) result
      | RFind_opt of string option
      | RFind_all of string list
      | RReplace of (unit, exn) result
      | RMem of bool
      | RLength of int
      | RClean [@@deriving show { with_path = false }]

    let init () = E.create 42
    let cleanup _ = ()
  end

module EConf =
  struct
    include Spec
    let run c e = match c with
      | Clear          -> E.clear e; RClear
      | Add (i, s)     -> E.add e i s; RAdd
      | Remove i       -> E.remove e i; RRemove
      | Find i         -> RFind (protect (E.find e) i)
      | Find_opt i     -> RFind_opt (E.find_opt e i)
      | Find_all i     -> RFind_all (E.find_all e i)
      | Replace (i, s) -> RReplace (protect (E.replace e i) s)
      | Mem i          -> RMem (E.mem e i)
      | Length         -> RLength (E.length e)
      | Clean          -> E.clean e; RClean
  end

module ET = Lin.Make(EConf)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main [
    ET.lin_test `Domain ~count:1000 ~name:"Ephemeron test with domains";
    ET.lin_test `Thread ~count:1000 ~name:"Ephemeron test with threads";
  ]
