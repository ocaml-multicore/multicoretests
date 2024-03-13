(* ********************************************************************** *)
(*                            Lin Tests [Weak]                            *)
(* ********************************************************************** *)
module WConf =
struct
  type t = int64 Weak.t

  let weak_size = 16
  let init () = Weak.create weak_size
  let cleanup _ = ()

  open Lin
  let int,int64 = nat_small,nat64_small
  let api =
    [ val_ "Weak.length"   Weak.length   (t @-> returning int);
      val_ "Weak.set"      Weak.set      (t @-> int @-> option int64 @-> returning_or_exc unit);
      val_ "Weak.get"      Weak.get      (t @-> int @-> returning_or_exc (option int64));
      val_ "Weak.get_copy" Weak.get_copy (t @-> int @-> returning_or_exc (option int64));
      val_ "Weak.check"    Weak.check    (t @-> int @-> returning_or_exc bool);
      val_ "Weak.fill"     Weak.fill     (t @-> int @-> int @-> option int64 @-> returning_or_exc unit);
    (*val blit : 'a t -> int -> 'a t -> int -> int -> unit *)
    ]
end

module WT_domain = Lin_domain.Make(WConf)
;;
QCheck_base_runner.run_tests_main [
  WT_domain.stress_test ~count:1000 ~name:"Lin Weak stress test with Domain";
]
