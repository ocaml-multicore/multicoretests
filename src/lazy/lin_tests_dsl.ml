(** parallel linearization tests of Lazy *)

(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
           else z

let work () =
  let r = ref 0 in
  for _ = 1 to 100 do
    r := !r + tak 18 12 6;
    (*assert (7 = tak 18 12 6);*)
  done;
  !r

(*
module Lazy :
  sig
    type 'a t = 'a CamlinternalLazy.t
    exception Undefined
    external force : 'a t -> 'a = "%lazy_force"
    val map : ('a -> 'b) -> 'a t -> 'b t
    val is_val : 'a t -> bool
    val from_val : 'a -> 'a t
    val map_val : ('a -> 'b) -> 'a t -> 'b t
    val from_fun : (unit -> 'a) -> 'a t
    val force_val : 'a t -> 'a
  end
*)

module LBase =
struct
  type t = int Lazy.t
  let cleanup _ = ()

  open Lin_api

  (* hack to work around missing function generators *)
  let fun_gen _ty _ty' =
    let print_fun _ = "Stdlib.succ" in
    let fun_gen = QCheck.(make ~print:print_fun (Gen.return Stdlib.succ)) in
    Lin_api.gen fun_gen print_fun

  let force_map f l = Lazy.force (Lazy.map f l)
  let force_map_val f l = Lazy.force (Lazy.map_val f l)
  let int = nat_small

  let api =
    [ val_ "Lazy.force"                Lazy.force     (t @-> returning_or_exc int);
      val_ "Lazy.force_val"            Lazy.force_val (t @-> returning_or_exc int);
      val_ "Lazy.is_val"               Lazy.is_val    (t @-> returning bool);
    (*val_ "Lazy.map"                  Lazy.map       (fun_gen int int @-> t @-> returning_or_exc t);*)
      val_ "Lazy.force o Lazy.map"     force_map      (fun_gen int int @-> t @-> returning_or_exc int);
    (*val_ "Lazy.map_val"              Lazy.map       (fun_gen int int @-> t @-> returning_or_exc t);*)
      val_ "Lazy.force o Lazy.map_val" force_map_val  (fun_gen int int @-> t @-> returning_or_exc int);
    ]
end

module LTlazyAPI = struct include LBase let init () = lazy (work ()) end
module LTlazy    = Lin_api.Make(LTlazyAPI)

module LTfromvalAPI = struct include LBase let init () = Lazy.from_val 42 end
module LTfromval = Lin_api.Make(LTfromvalAPI)

module LTfromfunAPI = struct include LBase let init () = Lazy.from_fun work end
module LTfromfun = Lin_api.Make(LTfromfunAPI)
;;
QCheck_base_runner.run_tests_main
  (let count = 100 in
   [LTlazy.neg_lin_test    `Domain ~count ~name:"Lin_api Lazy test with Domain";
    LTfromval.lin_test     `Domain ~count ~name:"Lin_api Lazy test with Domain from_val";
    LTfromfun.neg_lin_test `Domain ~count ~name:"Lin_api Lazy test with Domain from_fun";
   ])
