(* ************************************************************ *)
(*                Tests of thread-safe [Ephemeron]              *)
(*                                                              *)
(*    Note that while the API is immutable and does not have    *)
(*    any toplevel state, the test fails.                       *)
(*                                                              *)
(*    The present guess is that it is because of the GC.        *)
(*    The linearazibilty check fails because we don't have      *)
(*    control over the GC in order to reproduce its             *)
(*    behaviour.                                                *)
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

    open Lin_api
    let int = nat_small
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

module ET = Lin_api.Make(EConf)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main [
    ET.neg_lin_test `Domain ~count:1000 ~name:"Ephemeron test with domains";
    ET.lin_test     `Thread ~count:1000 ~name:"Ephemeron test with threads";
  ]
