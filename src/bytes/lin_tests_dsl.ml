(* ********************************************************************** *)
(*                      Tests of thread-unsafe [Bytes]                    *)
(* ********************************************************************** *)
module BConf = struct
  type t = Bytes.t
  let init () = Bytes.make 42 '0'
  let cleanup _ = ()

  open Lin_api
  let int = nat_small
(*let int = int_bound 10*)

  let api = [
    val_ "Bytes.length"      Bytes.length      (t @-> returning int);
    val_ "Bytes.get"         Bytes.get         (t @-> int @-> returning_or_exc char);
    val_ "Bytes.set"         Bytes.set         (t @-> int @-> char @-> returning_or_exc unit);
 (* val_ "Bytes.create"      Bytes.create      (int @-> returning_or_exc_ t); *) (* result contains arbitrary bytes, hence non-det. output! *)
    val_ "Bytes.make"        Bytes.make        (int @-> char @-> returning_or_exc_ t);
 (* val_ "Bytes.empty"       Bytes.empty       (returning_ t); *)
    val_ "Bytes.copy"        Bytes.copy        (t @-> returning_ t);
    val_ "Bytes.of_string"   Bytes.of_string   (string @-> returning_ t);
    val_ "Bytes.to_string"   Bytes.to_string   (t @-> returning string);
    val_ "Bytes.sub_string"  Bytes.sub_string  (t @-> int @-> int @-> returning_or_exc string);
    val_freq 2 "Bytes.fill"        Bytes.fill        (t @-> int @-> int @-> char @-> returning_or_exc unit);
    val_freq 2 "Bytes.blit_string" Bytes.blit_string (string @-> int @-> t @-> int @-> int @-> returning_or_exc unit);
    val_ "Bytes.trim"        Bytes.trim        (t @-> returning_ t);
    val_ "Bytes.escaped"     Bytes.escaped     (t @-> returning_ t);
    val_ "Bytes.index"       Bytes.index       (t @-> char @-> returning_or_exc int);
    val_ "Bytes.index_from"  Bytes.index_from  (t @-> int @-> char @-> returning_or_exc int)]
end

module BT = Lin_api.Make(BConf)
;;
Util.set_ci_printing ()
;;
QCheck_base_runner.run_tests_main [
  BT.neg_lin_test `Domain ~count:1000 ~name:"Lin_api Bytes test with Domain";
  BT.lin_test     `Thread ~count:250  ~name:"Lin_api Bytes test with Thread";
]
