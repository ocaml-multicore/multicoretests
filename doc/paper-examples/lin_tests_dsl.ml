(* ********************************************************************** *)
(*                      Tests of thread-unsafe [Hashtbl]                  *)
(* ********************************************************************** *)
module HashtblSig =
struct
  type t = (char, int) Hashtbl.t

  let init () = Hashtbl.create ~random:false 42
  let cleanup _ = ()

  open Lin_api
  let a,b = char_printable,nat_small
  let api =
    [ val_ "Hashtbl.clear"    Hashtbl.clear    (t @-> returning unit);
      val_ "Hashtbl.add"      Hashtbl.add      (t @-> a @-> b @-> returning unit);
      val_ "Hashtbl.remove"   Hashtbl.remove   (t @-> a @-> returning unit);
      val_ "Hashtbl.find"     Hashtbl.find     (t @-> a @-> returning_or_exc b);
      val_ "Hashtbl.replace"  Hashtbl.replace  (t @-> a @-> b @-> returning unit);
      val_ "Hashtbl.mem"      Hashtbl.mem      (t @-> a @-> returning bool);
      val_ "Hashtbl.length"   Hashtbl.length   (t @-> returning int);
    ]
end

module HT = Lin_api.Make(HashtblSig)
;;
QCheck_runner.run_tests_main [
  HT.lin_test     `Domain ~count:1000 ~name:"Hashtbl DSL test";
]
