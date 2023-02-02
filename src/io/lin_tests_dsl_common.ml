(* ********************************************************************** *)
(*                      Tests of in and out channels                      *)
(* ********************************************************************** *)

module ICConf : Lin.Spec = struct
  type t = In_channel.t

  let init () = In_channel.open_bin Sys.argv.(0)
  let cleanup = In_channel.close

  open Lin
  let int,int64 = nat_small,nat64_small

  let api = [
    (* Only one t is tested, so skip stdin and opening functions *)
    (* val_ "In_channel.stdin"               In_channel.stdin                 (t) ; *)
    (* val_ "In_channel.open_bin"            In_channel.open_bin              (string @-> t) ; *)
    (* val_ "In_channel.open_text"           In_channel.open_text             (string @-> t) ; *)
    (* val_ "In_channel.open_gen"            In_channel.open_gen              (open_flag list @-> int @-> string @-> t) ; *)
    (* val_ "In_channel.with_open_bin"       In_channel.with_open_bin         (string @-> (t @-> 'a) @-> 'a) ; *)
    (* val_ "In_channel.with_open_text"      In_channel.with_open_text        (string @-> (t @-> 'a) @-> 'a) ; *)
    (* val_ "In_channel.with_open_gen"       In_channel.with_open_gen         (open_flag list @-> int @-> string @-> (t @-> 'a) @-> 'a) ; *)

    val_ "In_channel.seek"                In_channel.seek                  (t @-> int64 @-> returning_or_exc unit) ;
    val_ "In_channel.pos"                 In_channel.pos                   (t @-> returning int64) ;
    val_ "In_channel.length"              In_channel.length                (t @-> returning_or_exc int64) ;
    val_ "In_channel.close"               In_channel.close                 (t @-> returning unit) ;
    val_ "In_channel.close_noerr"         In_channel.close_noerr           (t @-> returning unit) ;
    val_ "In_channel.input_char"          In_channel.input_char            (t @-> returning_or_exc (option char)) ;
    val_ "In_channel.input_byte"          In_channel.input_byte            (t @-> returning_or_exc (option int)) ;
    val_ "In_channel.input_line"          In_channel.input_line            (t @-> returning_or_exc (option string)) ;
    (* bytes not yet supported by the Lin library *)
    (* val_ "In_channel.input"               In_channel.input                 (t @-> bytes @-> int @-> int @-> returning int) ; *)
    (* val_ "In_channel.really_input"        In_channel.really_input          (t @-> bytes @-> int @-> int @-> returning (option unit)) ; *)
    val_ "In_channel.really_input_string" In_channel.really_input_string   (t @-> int @-> returning_or_exc (option string)) ;
    (* input_all generates counter-examples that are impossibly long *)
    (* val_ "In_channel.input_all"           In_channel.input_all             (t @-> returning_or_exc string) ; *)
    val_ "In_channel.set_binary_mode"     In_channel.set_binary_mode       (t @-> bool @-> returning_or_exc unit) ;
  ]
end

module OCConf : Lin.Spec = struct
  (* a path and an open channel to that file; we need to keep the path
     to cleanup after the test run *)
  type t = string * Out_channel.t

  let init () = Filename.open_temp_file "lin-dsl-" ""
  let cleanup (path, chan) =
    Out_channel.close chan ;
    Sys.remove path

  (* turn [f: Out_channel.t -> ...] into [lift f: t -> ...] *)
  let lift f (_, chan) = f chan

  open Lin
  let int,int64,string = nat_small,nat64_small,string_small
  let api = [
    (* Only one t is tested, so skip stdout, stderr and opening functions *)

    (* val_ "Out_channel.stdout"           Out_channel.stdout           (t) ; *)
    (* val_ "Out_channel.stderr"           Out_channel.stderr           (t) ; *)
    (* val_ "Out_channel.open_bin"         Out_channel.open_bin         (string @-> returning t) ; *)
    (* val_ "Out_channel.open_text"        Out_channel.open_text        (string @-> returning t) ; *)
    (* val_ "Out_channel.open_gen"         Out_channel.open_gen         (open_flag list @-> int @-> string @-> returning t) ; *)
    (* val_ "Out_channel.with_open_bin"    Out_channel.with_open_bin    (string @-> (t @-> 'a) @-> returning 'a) ; *)
    (* val_ "Out_channel.with_open_text"   Out_channel.with_open_text   (string @-> (t @-> 'a) @-> returning 'a) ; *)
    (* val_ "Out_channel.with_open_gen"    Out_channel.with_open_gen    (open_flag list @-> int @-> string @-> (t @-> 'a) @-> returning 'a) ; *)

    val_ "Out_channel.seek"             (lift Out_channel.seek)             (t @-> int64 @-> returning_or_exc unit) ;
    val_ "Out_channel.pos"              (lift Out_channel.pos)              (t @-> returning_or_exc int64) ;
    val_ "Out_channel.length"           (lift Out_channel.length)           (t @-> returning_or_exc int64) ;
    val_ "Out_channel.close"            (lift Out_channel.close)            (t @-> returning_or_exc unit) ;
    val_ "Out_channel.close_noerr"      (lift Out_channel.close_noerr)      (t @-> returning unit) ;
    val_ "Out_channel.flush"            (lift Out_channel.flush)            (t @-> returning_or_exc unit) ;
    val_ "Out_channel.flush_all"              Out_channel.flush_all         (unit @-> returning_or_exc unit) ;
    val_ "Out_channel.output_char"      (lift Out_channel.output_char)      (t @-> char @-> returning_or_exc unit) ;
    val_ "Out_channel.output_byte"      (lift Out_channel.output_byte)      (t @-> int @-> returning_or_exc unit) ;
    val_ "Out_channel.output_string"    (lift Out_channel.output_string)    (t @-> string @-> returning_or_exc unit) ;

    (* val_ "Out_channel.output_bytes"     Out_channel.output_bytes     (t @-> bytes @-> returning unit) ; *)
    (* val_ "Out_channel.output"           Out_channel.output           (t @-> bytes @-> int @-> int @-> returning unit) ; *)

    val_ "Out_channel.output_substring" (lift Out_channel.output_substring) (t @-> string @-> int @-> int @-> returning_or_exc unit) ;
    val_ "Out_channel.set_binary_mode"  (lift Out_channel.set_binary_mode)  (t @-> bool @-> returning_or_exc unit) ;
    val_ "Out_channel.set_buffered"     (lift Out_channel.set_buffered)     (t @-> bool @-> returning_or_exc unit) ;
    val_ "Out_channel.is_buffered"      (lift Out_channel.is_buffered)      (t @-> returning_or_exc bool) ;
  ]
end
