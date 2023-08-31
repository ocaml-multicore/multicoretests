(* Tests of Util.Pp *)

open Util.Pp

let pr name pp x = Printf.printf "Test of %s:\n%s\n\n" name (to_show pp x)

let seq_interval x y () =
  let rec aux i =
    let open Seq in
    if i <= y then Cons (i, fun () -> aux (i + 1)) else Nil
  in
  aux x

let _ =
  pr "pp_bool" pp_bool true;
  pr "pp_int (positive)" pp_int 12345;
  pr "pp_int (negative)" pp_int (-12345);
  pr "pp_int32 (positive)" pp_int32 12345l;
  pr "pp_int64 (negative)" pp_int64 (-12345L);
  pr "pp_float (infinity)" pp_float Float.infinity;
  pr "pp_float (pi)" pp_float Float.pi;
  pr "pp_char (printable)" pp_char 'a';
  pr "pp_char (unprintable)" pp_char '\000';
  pr "pp_string" pp_string "Hello world";
  pr "pp_string (long)" pp_string (String.make 1234 'a');
  pr "pp_bytes (empty)" pp_bytes Bytes.empty;
  pr "pp_bytes (long)" pp_bytes (Bytes.make 1234 'b');
  pr "pp_option pp_int (positive)" (pp_option pp_int) (Some 12345);
  pr "pp_option pp_int (negative)" (pp_option pp_int) (Some (-12345));
  pr "pp_result pp_int pp_string" (pp_result pp_int pp_string) (Ok (-12345));
  pr "pp_result pp_int pp_string" (pp_result pp_int pp_string) (Error "Failure");
  pr "pp_pair pp_char pp_int" (pp_pair pp_char pp_int) ('a', -12345);
  let l = [ 1; 2; 3; -1; -2; -3 ] in
  pr "pp_list pp_int" (pp_list pp_int) l;
  let l = l @ l @ l @ l in
  let l = l @ l @ l @ l in
  let l = l @ l @ l @ l in
  pr "pp_list pp_int (long)" (pp_list pp_int) l;
  pr "pp_seq pp_int" (pp_seq pp_int) (seq_interval (-5) 5);
  pr "pp_seq pp_int (long)" (pp_seq pp_int) (seq_interval (-50) 50);
  pr "pp_array pp_int" (pp_array pp_int) [| 1; 2; 3; -1; -2; -3 |];
  pr "pp_array pp_int (long)" (pp_array pp_int) (Array.make 100 0);
  pr "pp_record" pp_record
    [ pp_field "key" pp_int 123; pp_field "value" pp_string "content" ]
