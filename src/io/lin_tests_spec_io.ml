(* ********************************************************************** *)
(*                      Tests of in and out channels                      *)
(* ********************************************************************** *)

(* Try to find a text file rather than a binary to test In_channel, so
   that input_line makes more sense *)
let in_file =
  let path0 = Filename.dirname Sys.executable_name
  and up p = Filename.concat p Filename.parent_dir_name
  and candidate p = Filename.concat p "dune" in
  let path1 = up path0 in
  let path2 = up path1 in
  let path3 = up path2 in
  let path4 = up path3 in
  let candidates = List.map candidate [ path0; path1; path2; path3; path4 ] in
  let existing = List.filter Sys.file_exists candidates in
  match existing with
  | [] -> Sys.executable_name
  | f :: _ -> f

module ICConf : Lin.Spec = struct
  type t = In_channel.t

  let init () = In_channel.open_bin in_file
  let cleanup = In_channel.close

  open Lin
  let int,int64 = nat_small,nat64_small

  let bytes =
    let open QCheck in
    let zeroed_bytes n = Bytes.make n '\000' in
    let shrink b = Iter.map zeroed_bytes (Shrink.int (Bytes.length b))
    and gen = Gen.map zeroed_bytes Gen.small_nat in
    let bytes = make ~shrink ~small:Bytes.length ~print:Print.bytes gen in
    gen_deconstructible bytes (print Lin.bytes) Bytes.equal

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
    val_ "In_channel.input"               In_channel.input                 (t @-> bytes @-> int @-> int @-> returning_or_exc int) ;
    val_ "In_channel.really_input"        In_channel.really_input          (t @-> bytes @-> int @-> int @-> returning_or_exc (option unit)) ;
    val_ "In_channel.really_input_string" In_channel.really_input_string   (t @-> int @-> returning_or_exc (option string)) ;
    (* input_all generates counter-examples that are impossibly long *)
    (* val_ "In_channel.input_all"           In_channel.input_all             (t @-> returning_or_exc string) ; *)
    val_ "In_channel.set_binary_mode"     In_channel.set_binary_mode       (t @-> bool @-> returning_or_exc unit) ;
  ]
end

module OCConf : Lin.Spec = struct
  (* a path and an open channel to that file; we need to keep the path
     to cleanup after the test run *)
  type t = Out_channel.t
  let path = ref ""

  let init () =
    let p,ch = Filename.open_temp_file "lin-dsl-" "" in
    path := p;
    ch

  let cleanup chan =
    Out_channel.close chan;
    Sys.remove !path

  open Lin
  let int,int64 = nat_small,nat64_small

  (* disable string and bytes char shrinking as too many shrinking candidates
     triggers long Out_channel shrink runs on Mingw + Cygwin *)
  let string =
    let string = QCheck.(set_shrink Shrink.(string ~shrink:nil) string_small) in
    gen_deconstructible string (print Lin.string) String.equal
  let bytes =
    let bytes = QCheck.(set_shrink Shrink.(bytes ~shrink:nil) bytes_small) in
    gen_deconstructible bytes (print Lin.bytes) Bytes.equal

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

    val_freq 6 "Out_channel.seek"             Out_channel.seek             (t @-> int64 @-> returning_or_exc unit) ;
    val_freq 9 "Out_channel.pos"              Out_channel.pos              (t @-> returning_or_exc int64) ;
    val_freq 9 "Out_channel.length"           Out_channel.length           (t @-> returning_or_exc int64) ;
    val_freq 6 "Out_channel.close"            Out_channel.close            (t @-> returning_or_exc unit) ;
    val_freq 6 "Out_channel.close_noerr"      Out_channel.close_noerr      (t @-> returning unit) ;
    val_freq 6 "Out_channel.flush"            Out_channel.flush            (t @-> returning_or_exc unit) ;
    val_freq 1 "Out_channel.flush_all"        Out_channel.flush_all        (unit @-> returning_or_exc unit) ;
    val_freq 6 "Out_channel.output_char"      Out_channel.output_char      (t @-> char @-> returning_or_exc unit) ;
    val_freq 6 "Out_channel.output_byte"      Out_channel.output_byte      (t @-> int @-> returning_or_exc unit) ;
    val_freq 6 "Out_channel.output_string"    Out_channel.output_string    (t @-> string @-> returning_or_exc unit) ;
    val_freq 6 "Out_channel.output_bytes"     Out_channel.output_bytes     (t @-> bytes @-> returning_or_exc unit) ;
    val_freq 6 "Out_channel.output"           Out_channel.output           (t @-> bytes @-> int @-> int @-> returning_or_exc unit) ;
    val_freq 6 "Out_channel.output_substring" Out_channel.output_substring (t @-> string @-> int @-> int @-> returning_or_exc unit) ;
    val_freq 6 "Out_channel.set_binary_mode"  Out_channel.set_binary_mode  (t @-> bool @-> returning_or_exc unit) ;
    val_freq 6 "Out_channel.set_buffered"     Out_channel.set_buffered     (t @-> bool @-> returning_or_exc unit) ;
    val_freq 6 "Out_channel.is_buffered"      Out_channel.is_buffered      (t @-> returning_or_exc bool) ;
  ]
end
