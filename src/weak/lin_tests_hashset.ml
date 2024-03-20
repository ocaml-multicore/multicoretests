(* ********************************************************************** *)
(*                       Lin tests of [Weak Hashset]                      *)
(* ********************************************************************** *)
module WHSConf =
struct
  module WHS = Weak.Make(String)
  type t = WHS.t
  let weak_size = 16
  let init () = WHS.create weak_size
  let cleanup t = WHS.clear t

  open Lin
  let string = string_small
  let api =
    [ val_ "Weak.S.clear"    WHS.clear    (t @-> returning unit);
      val_ "Weak.S.merge"    WHS.merge    (t @-> string @-> returning_or_exc string);
      val_ "Weak.S.add"      WHS.add      (t @-> string @-> returning_or_exc unit);
      val_ "Weak.S.remove"   WHS.remove   (t @-> string @-> returning_or_exc unit);
      val_ "Weak.S.find"     WHS.find     (t @-> string @-> returning_or_exc string);
      val_ "Weak.S.find_opt" WHS.find_opt (t @-> string @-> returning_or_exc (option string));
      val_ "Weak.S.find_all" WHS.find_all (t @-> string @-> returning_or_exc (list string));
      val_ "Weak.S.mem"      WHS.mem      (t @-> string @-> returning_or_exc bool);
    (*val iter : (data -> unit) -> t -> unit*)
    (*val fold : (data -> 'a -> 'a) -> t -> 'a -> 'a*)
      val_ "Weak.S.count"    WHS.count    (t @-> returning int);
    (*val stats : t -> int * int * int * int * int * int*)
    ]
end

module WHST_domain = Lin_domain.Make(WHSConf)
;;
QCheck_base_runner.run_tests_main [
  WHST_domain.stress_test ~count:1000 ~name:"Lin Weak HashSet stress test with Domain";
]
