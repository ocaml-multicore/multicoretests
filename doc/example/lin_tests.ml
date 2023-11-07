(* ********************************************************************** *)
(*                      Tests of thread-unsafe [Hashtbl]                  *)
(* ********************************************************************** *)
module HashtblSig =
struct
  type t = (char, int) Hashtbl.t
  let init () = Hashtbl.create ~random:false 42
  let cleanup _ = ()

  open Lin
  let a,b = char_printable,nat_small
  let api =
    [ val_ "Hashtbl.add"    Hashtbl.add    (t @-> a @-> b @-> returning unit);
      val_ "Hashtbl.remove" Hashtbl.remove (t @-> a @-> returning unit);
      val_ "Hashtbl.find"   Hashtbl.find   (t @-> a @-> returning_or_exc b);
      val_ "Hashtbl.mem"    Hashtbl.mem    (t @-> a @-> returning bool);
      val_ "Hashtbl.length" Hashtbl.length (t @-> returning int); ]
end

module HT = Lin_domain.Make(HashtblSig)
;;
QCheck_base_runner.run_tests_main [
  HT.lin_test ~count:1000 ~name:"Lin Hashtbl test";
]
