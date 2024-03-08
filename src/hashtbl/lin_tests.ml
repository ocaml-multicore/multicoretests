(* ********************************************************************** *)
(*                      Tests of thread-unsafe [Hashtbl]                  *)
(* ********************************************************************** *)
module HConf (*: Lin.ApiSpec*) =
struct
  type t = (char, int) Hashtbl.t

  let init () = Hashtbl.create ~random:false 42
  let cleanup _ = ()

  open Lin
  let int,char = nat_small,char_printable
  let api =
    [ val_ "Hashtbl.clear"    Hashtbl.clear    (t @-> returning unit);
      val_ "Hashtbl.add"      Hashtbl.add      (t @-> char @-> int @-> returning unit);
      val_ "Hashtbl.remove"   Hashtbl.remove   (t @-> char @-> returning unit);
      val_ "Hashtbl.find"     Hashtbl.find     (t @-> char @-> returning_or_exc int);
      val_ "Hashtbl.find_opt" Hashtbl.find_opt (t @-> char @-> returning (option int));
      val_ "Hashtbl.find_all" Hashtbl.find_all (t @-> char @-> returning (list int));
      val_ "Hashtbl.replace"  Hashtbl.replace  (t @-> char @-> int @-> returning unit);
      val_ "Hashtbl.mem"      Hashtbl.mem      (t @-> char @-> returning bool);
      val_ "Hashtbl.length"   Hashtbl.length   (t @-> returning int);
    ]
end

module HT_domain = Lin_domain.Make(HConf)
;;
QCheck_base_runner.run_tests_main [
  HT_domain.neg_lin_test ~count:1000 ~name:"Lin Hashtbl test with Domain";
  HT_domain.stress_test  ~count:1000 ~name:"Lin Hashtbl stress test with Domain";
]
