(* ********************************************************************** *)
(*                      Tests of thread-unsafe [Weak]                     *)
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
  WT_domain.neg_lin_test   ~count:1000 ~name:"Lin DSL Weak test with Domain";
  WHST_domain.neg_lin_test ~count:1000 ~name:"Lin DSL Weak HashSet test with Domain";
]
