module Buffer = BaseOrNoBase.Buffer

module BBConf = struct
  type t = Buffer.t

  let init () = Buffer.create 8
  let cleanup _ = ()

  open Lin
  let int = nat_small
  let char = char_printable
  let string = string_small_printable

  let api =
    [
      (* val_ "Buffer.sexp_of_t" Buffer.sexp_of_t (t @-> returning Sexplib0.Sexp.t); *)
          (* Disabled due to missing Sexplib0.Sexp.t *)
      (* val_ "Buffer.create" Buffer.create (int @-> returning t); *)
          (* Disabled as returning t *)

      val_ "Buffer.contents" Buffer.contents (t @-> returning_or_exc string);

      (* val_ "Buffer.contents_bytes" Buffer.contents_bytes (t @-> returning bytes); *)
          (* Disabled due to missing bytes *)

      val_ "Buffer.nth" Buffer.nth (t @-> int @-> returning_or_exc char);
      val_ "Buffer.length" Buffer.length (t @-> returning int);
      val_ "Buffer.clear" Buffer.clear (t @-> returning unit);
      val_ "Buffer.reset" Buffer.reset (t @-> returning unit);
      val_ "Buffer.add_char" Buffer.add_char (t @-> char @-> returning_or_exc unit);
      val_ "Buffer.add_string" Buffer.add_string (t @-> string @-> returning_or_exc unit);

      (* val_ "Buffer.add_substring" Buffer.add_substring (t @-> string @-> int @-> int @-> returning_or_exc unit); *)
          (* Disabled due to missing labels *)

      (* val_ "Buffer.add_bytes" Buffer.add_bytes (t @-> bytes @-> returning unit); *)
      (* val_ "Buffer.add_subbytes" Buffer.add_subbytes (t @-> bytes @-> int @-> int @-> returning unit); *)
          (* Disabled due to missing bytes *)

      (* val_ "Buffer.add_buffer" Buffer.add_buffer (t @-> t @-> unit); *)
    ]
end

module BBT_domain = Lin_domain.Make(BBConf)

let _ =
  QCheck_base_runner.run_tests_main
    [ BBT_domain.neg_lin_test ~count:1000 ~name:"Lin DSL Base Buffer test with Domain" ]
