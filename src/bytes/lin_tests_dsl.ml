(* ********************************************************************** *)
(*                      Tests of thread-unsafe [Bytes]                    *)
(* ********************************************************************** *)
module BConf = struct
  type t = Bytes.t
  let init () = Stdlib.Bytes.make 42 '0'
  let cleanup _ = ()

  open Lin

  let api = [
    val_ "Bytes.get"         Bytes.get         (t @-> int @-> returning_or_exc char) ~pure:true;
    val_ "Bytes.sub_string"  Bytes.sub_string  (t @-> int @-> int @-> returning_or_exc string) ~pure:true;
    val_ "Bytes.length"      Bytes.length      (t @-> returning int) ~pure:true;
    val_ "Bytes.fill"        Bytes.fill        (t @-> int @-> int @-> char @-> returning_or_exc unit);
    val_ "Bytes.blit_string" Bytes.blit_string (string @-> int @-> t @-> int @-> int @-> returning_or_exc unit);
    val_ "Bytes.index_from"  Bytes.index_from  (t @-> int @-> char @-> returning_or_exc int) ~pure:true]
end

module BT_domain = Lin_domain.Make(BConf)
module BT_thread = Lin_thread.Make(BConf) [@alert "-experimental"]
;;
QCheck_base_runner.run_tests_main [
  BT_domain.lin_test ~count:1000 ~name:"Lin DSL Bytes test with Domain";
  BT_thread.lin_test ~count:1000 ~name:"Lin DSL Bytes test with Thread";
]
