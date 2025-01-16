(* ********************************************************************** *)
(*                      Tests of thread-unsafe [Bytes]                    *)
(* ********************************************************************** *)
module BConf = struct
  type t = Bytes.t
  let init () = Stdlib.Bytes.make 42 '0'
  let cleanup _ = ()

  open Lin
  let int,string = nat_small, string_small_printable
  let api = [
    val_ "Bytes.length"            Bytes.length            (t @-> returning int);
    val_ "Bytes.get"               Bytes.get               (t @-> int @-> returning_or_exc char);
    val_ "Bytes.set"               Bytes.set               (t @-> int @-> char @-> returning_or_exc unit);
    val_ "Bytes.copy"              Bytes.copy              (t @-> returning bytes);
    val_ "Bytes.to_string"         Bytes.to_string         (t @-> returning string);
    val_ "Bytes.sub"               Bytes.sub               (t @-> int @-> int @-> returning_or_exc bytes);
    val_ "Bytes.sub_string"        Bytes.sub_string        (t @-> int @-> int @-> returning_or_exc string);
    val_ "Bytes.fill"              Bytes.fill              (t @-> int @-> int @-> char @-> returning_or_exc unit);
    val_ "Bytes.blit_string"       Bytes.blit_string       (string @-> int @-> t @-> int @-> int @-> returning_or_exc unit);
    val_ "Bytes.index"             Bytes.index             (t @-> char @-> returning_or_exc int);
    val_ "Bytes.index_opt"         Bytes.index_opt         (t @-> char @-> returning (option int));
    val_ "Bytes.rindex"            Bytes.rindex            (t @-> char @-> returning_or_exc int);
    val_ "Bytes.rindex_opt"        Bytes.rindex_opt        (t @-> char @-> returning (option int));
    val_ "Bytes.index_from"        Bytes.index_from        (t @-> int @-> char @-> returning_or_exc int);
    val_ "Bytes.index_from_opt"    Bytes.index_from_opt    (t @-> int @-> char @-> returning_or_exc (option int));
    val_ "Bytes.rindex_from"       Bytes.rindex_from       (t @-> int @-> char @-> returning_or_exc int);
    val_ "Bytes.rindex_from_opt"   Bytes.rindex_from_opt   (t @-> int @-> char @-> returning_or_exc (option int));
    val_ "Bytes.contains"          Bytes.contains          (t @-> char @-> returning_or_exc bool);
    val_ "Bytes.contains_from"     Bytes.contains_from     (t @-> int @-> char @-> returning_or_exc bool);
    val_ "Bytes.rcontains_from"    Bytes.rcontains_from    (t @-> int @-> char @-> returning_or_exc bool);
    val_ "Bytes.is_valid_utf_8"    Bytes.is_valid_utf_8    (t @-> returning bool);
    val_ "Bytes.is_valid_utf_16be" Bytes.is_valid_utf_16be (t @-> returning bool);
    val_ "Bytes.is_valid_utf_16le" Bytes.is_valid_utf_16le (t @-> returning bool);
  ]
end

module BT_domain = Lin_domain.Make(BConf)
module BT_thread = Lin_thread.Make(BConf) [@alert "-experimental"]
;;
QCheck_base_runner.run_tests_main [
  BT_domain.neg_lin_test ~count:2500 ~name:"Lin Bytes test with Domain";
  BT_thread.lin_test     ~count:250  ~name:"Lin Bytes test with Thread";
]
