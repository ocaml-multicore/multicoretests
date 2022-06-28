(* ********************************************************************** *)
(*                      Tests of thread-unsafe [Bytes]                    *)
(* ********************************************************************** *)
module BConf = struct
  type t = Bytes.t
  let init () = Stdlib.Bytes.make 42 '0'
  let cleanup _ = ()

  open Lin_api
  let api = [
    val_ "Bytes.get"         Bytes.get         (t @-> int @-> returning_or_exc char);
    val_ "Bytes.sub_string"  Bytes.sub_string  (t @-> int @-> int @-> returning_or_exc string);
    val_ "Bytes.length"      Bytes.length      (t @-> returning int);
    val_ "Bytes.fill"        Bytes.fill        (t @-> int @-> int @-> char @-> returning_or_exc unit);
    val_ "Bytes.blit_string" Bytes.blit_string (string @-> int @-> t @-> int @-> int @-> returning_or_exc unit);
    val_ "Bytes.index_from"  Bytes.index_from  (t @-> int @-> char @-> returning_or_exc int)]
end

module BT = Lin_api.Make(BConf)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main [
  BT.lin_test `Domain ~count:1000 ~name:"Bytes test";
  BT.lin_test `Thread ~count:1000 ~name:"Bytes test";
]
