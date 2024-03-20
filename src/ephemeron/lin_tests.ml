(* ************************************************************ *)
(*                   Lin tests of [Ephemeron]                   *)
(* ************************************************************ *)

module EConf =
  struct
     module E = Ephemeron.K1.Make(struct
                   type t = Int.t
                   let equal = Int.equal
                   let hash = Fun.id
                 end)

    type t = string E.t
    let init () = E.create 42
    let cleanup _ = ()

    open Lin
    let int,string = nat_small, string_small_printable
    let api =
      [ val_ "Ephemeron.clear"    E.clear    (t @-> returning unit);
        val_ "Ephemeron.add"      E.add      (t @-> int @-> string @-> returning unit);
        val_ "Ephemeron.remove"   E.remove   (t @-> int @-> returning unit);
        val_ "Ephemeron.find"     E.find     (t @-> int @-> returning_or_exc string);
        val_ "Ephemeron.find_opt" E.find_opt (t @-> int @-> returning (option string));
        val_ "Ephemeron.find_all" E.find_all (t @-> int @-> returning (list string));
        val_ "Ephemeron.replace"  E.replace  (t @-> int @-> string @-> returning unit);
        val_ "Ephemeron.mem"      E.mem      (t @-> int @-> returning bool);
        val_ "Ephemeron.length"   E.length   (t @-> returning int);
        val_ "Ephemeron.clean"    E.clean    (t @-> returning unit);
      ]
  end

module ET_domain = Lin_domain.Make(EConf)
;;
QCheck_base_runner.run_tests_main [
  ET_domain.stress_test ~count:1000 ~name:"Lin Ephemeron stress test with Domain";
]
