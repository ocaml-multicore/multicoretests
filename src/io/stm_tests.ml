open QCheck
open STM

(** parallel STM tests of Out_channels *)

module OCConf =
struct
  type cmd =
    | Open_text
    | Seek of int64
    | Pos
    | Length
    | Close
    | Close_noerr
    | Flush
    | Output_char of char
    | Output_byte of int
    | Output_string of string
    | Output_bytes of bytes
    | Output of bytes * int * int
    | Output_substring of string * int * int

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Open_text -> cst0 "Open_text" fmt
    | Seek i -> cst1 pp_int64 "Seek" par fmt i
    | Pos -> cst0 "Pos" fmt
    | Length -> cst0 "Length" fmt
    | Close -> cst0 "Close" fmt
    | Close_noerr -> cst0 "Close_noerr" fmt
    | Flush -> cst0 "Flush" fmt
    | Output_char c -> cst1 pp_char "Output_char" par fmt c
    | Output_byte i -> cst1 pp_int "Output_byte" par fmt i
    | Output_string s -> cst1 pp_string "Output_string" par fmt s
    | Output_bytes b -> cst1 pp_bytes "Output_bytes" par fmt b
    | Output (b,p,l) -> cst3 pp_bytes pp_int pp_int "Output" par fmt b p l
    | Output_substring (s,p,l) -> cst3 pp_string pp_int pp_int "Output_substring" par fmt s p l

  let show_cmd = Util.Pp.to_show pp_cmd

  (* a path and an open channel to that file; we need to keep the path
     to cleanup after the test run *)
  type sut = { path            : string;
               mutable channel : Out_channel.t }

  (*type status = Opened | Closed*)
  type state = Closed of int64
             | Open of { position : int64;
                         length   : int64; }

  let arb_cmd s =
    let int64_gen = Gen.(map Int64.of_int small_int) in
    let char_gen = Gen.printable in
    let byte_gen = Gen.small_int in
    let string_gen = Gen.small_string in
    let bytes_gen = Gen.bytes_small in
    QCheck.make ~print:show_cmd (*~shrink:shrink_cmd*)
      (match s with
       | Closed _ -> Gen.return Open_text (* close can trigger a fatal error *)
       | Open _ ->
         Gen.(frequency [
             (*1,return Open_text;*)
             5,map (fun i -> Seek i) int64_gen;
             5,return Pos;
             5,return Length;
             1,return Close;
             1,return Close_noerr;
             5,return Flush;
             5,map (fun c -> Output_char c) char_gen;
             5,map (fun i -> Output_byte i) byte_gen;
             5,map (fun c -> Output_string c) string_gen;
             5,map (fun b -> Output_bytes b) bytes_gen;
             5,map3 (fun b p l -> Output (b,p,l)) bytes_gen byte_gen byte_gen;
             5,map3 (fun s p l -> Output_substring (s,p,l)) string_gen byte_gen byte_gen;
           ]))

  let init_state  = Closed 0L (*Open { position = 0L; length = 0L }*)

  let next_state c s = match c,s with
    | Open_text, Closed _l -> Open { position = 0L; length = (*l*) 0L }
    | Open_text, Open _ -> s
    | Seek _, Closed _ -> s
    | Seek p, Open { position = _; length } -> Open { position = p; length = Int64.max length p }
    | Pos,_ -> s
    | Length,_ -> s
    | Close, Open { position = _; length = _ } -> Closed 0L
    | Close, Closed _ -> s
    | Close_noerr, Open { position = _; length = _ } -> Closed 0L
    | Close_noerr, Closed _ -> s
    | Flush, Open _ -> s
    | Flush, Closed _ -> s
    | Output_char _c, Closed _ -> s
    | Output_char _c, Open { position; length } ->
       Open {position = Int64.succ position;
             length   = Int64.succ length; }
    | Output_byte _i, Closed _ -> s
    | Output_byte _i, Open { position; length } ->
       Open {position = Int64.succ position;
             length   = Int64.succ length; }
    | Output_string _str, Closed _ -> s
    | Output_string str, Open { position; length } ->
       let len = Int64.of_int (String.length str) in
       Open {position = Int64.add position len;
             length   = Int64.add length len; }
    | Output_bytes _str, Closed _ -> s
    | Output_bytes b, Open { position; length } ->
       let len = Int64.of_int (Bytes.length b) in
       Open {position = Int64.add position len;
             length   = Int64.add length len; }
    | Output (_,_,_), Closed _ -> s
    | Output (b,p,l), Open { position; length } ->
      let bytes_len = Bytes.length b in
      if 0 <= p && p < bytes_len &&
         0 <= l && p+l <= bytes_len
      then
        let len = Int64.of_int l in
        Open {position = Int64.add position len;
              length   = Int64.add length len; }
      else s
    | Output_substring (_,_,_), Closed _ -> s
    | Output_substring (str,p,l), Open { position; length } ->
      let str_len = String.length str in
      if 0 <= p && p < str_len &&
         0 <= l && p+l <= str_len
      then
        let len = Int64.of_int l in
        Open {position = Int64.add position len;
              length   = Int64.add length len; }
      else s

  let init_sut () =
    let path = Filename.temp_file "lin-dsl-" "" in
    (*Printf.printf "init_sut %s\n%!" path;*)
    let channel = Stdlib.stdout in
    (*let path, channel = Filename.open_temp_file "lin-dsl-" "" in*)
    { path; channel }

  let cleanup { path; channel } =
    if channel <> Stdlib.stdout
    then Out_channel.close channel;
    (*Printf.printf "Removing %s\n%!" path;*)
    Sys.remove path

  let precond c s = match c,s with
    | Open_text, Closed _ -> true
    | Open_text, Open _ -> false
    | _, Open _ -> true
    | _, _ -> false

  let run c ({path;channel = oc} as r) = match c with
    | Open_text       -> Res (result unit exn, protect (fun path -> (r.channel <- Out_channel.open_text path;())) path)
    | Seek p          -> Res (unit, Out_channel.seek oc p)
    | Pos             -> Res (result int64 exn, protect Out_channel.pos oc)
    | Length          -> Res (int64, Out_channel.length oc)
    | Close           -> Res (result unit exn, protect Out_channel.close oc)
    | Close_noerr     -> Res (unit, Out_channel.close_noerr oc)
    | Flush           -> Res (unit, Out_channel.flush oc)
    | Output_char c   -> Res (result unit exn, protect (Out_channel.output_char oc) c)
    | Output_byte i   -> Res (result unit exn, protect (Out_channel.output_byte oc) i)
    | Output_string s -> Res (result unit exn, protect (Out_channel.output_string oc) s)
    | Output_bytes b  -> Res (result unit exn, protect (Out_channel.output_bytes oc) b)
    | Output (b,p,l)  -> Res (result unit exn, protect (Out_channel.output oc b p) l)
    | Output_substring (s,p,l) -> Res (result unit exn, protect (Out_channel.output_substring oc s p) l)

  let postcond c (s:state) res = match c, res with
    | Open_text, Res ((Result (Unit,Exn),_), r) ->
       (match s,r with
        | Closed _, Ok ()
        | Closed _, Error (Sys_error _) (*"/tmp/lin-dsl-03ba23: Too many open files"*)
        | Open _, Ok ()
        | Open _, Error (Sys_error _) -> true
        | _ -> false)
    | Seek _, Res ((Unit,_), ()) -> true
    | Pos, Res ((Result (Int64,Exn),_), r) ->
       (match s with
        | Closed _ -> true (*r = Error (Invalid_argument "Pos exception") - unspecified *)
        | Open {position;length = _} -> r = Ok position)
    | Length, Res ((Int64,_),i) ->
       (*Printf.printf "Length returned %Li\n%!" i;*)
       (match s with
        | Closed _ -> true
        | Open { position = _; length } -> i <= length)
    | Close, Res ((Result (Unit,Exn),_), r) ->
       (match s,r with
         | Closed _, Error (Sys_error _) (*"Close exception" - unspecified *)
         | Open {position = _; length = _}, Ok () -> true
         | _ -> false)
    | Close_noerr, Res ((Unit,_), r) ->
       (match s,r with
         | Closed _, ()
         | Open {position = _; length = _}, () -> true)
    | Flush, Res ((Unit,_), r) -> r = ()
    | Output_char _c, Res ((Result (Unit,Exn),_), r) ->
       (match s with
        | Closed _ -> true
        | Open _ -> r = Ok ()) (* print on closed unspecified *)
    | Output_byte _i, Res ((Result (Unit,Exn),_), r) ->
       (match s with
        | Closed _ -> true
        | Open _ -> r = Ok ()) (* print on closed unspecified *)
    | Output_string _s, Res ((Result (Unit,Exn),_), r) ->
       (match s with
        | Closed _ -> true
        | Open _ -> r = Ok ()) (* print on closed unspecified *)
    | Output_bytes _b, Res ((Result (Unit,Exn),_), r) ->
       (match s with
        | Closed _ -> true
        | Open _ -> r = Ok ()) (* print on closed unspecified *)
    | Output (b,p,l), Res ((Result (Unit,Exn),_), r) ->
       (match s,r with
        | Closed _,_
        | Open _, Ok () -> true
        | Open _, Error (Invalid_argument _) -> (*"output"*)
          let bytes_len = Bytes.length b in
          p < 0 || p >= bytes_len || l < 0 || p+l > bytes_len
        | Open _, _ -> false)
    | Output_substring (str,p,l), Res ((Result (Unit,Exn),_), r) ->
       (match s,r with
        | Closed _,_
        | Open _, Ok () -> true
        | Open _, Error (Invalid_argument _) -> (*"output_substring"*)
          let str_len = String.length str in
          p < 0 || p >= str_len || l < 0 || p+l > str_len
        | Open _, _ -> false)
    | _, _ -> false
end

module OCSTM_seq = STM_sequential.Make(OCConf)
module OCSTM_dom = STM_domain.Make(OCConf)

let print_seq_trace trace =
  List.fold_left
    (fun acc c -> Printf.sprintf "%s\n   %s" acc (OCConf.show_cmd c))
    "" trace

let _seq_agree_test =
  Test.make ~count:1000 ~name:"STM Out_channel test sequential"
    (OCSTM_seq.arb_cmds OCConf.init_state)
    (fun seq ->
      try
        Printf.printf "%s\n%!" (print_seq_trace seq);
        let r = OCSTM_seq.agree_prop seq in
        Printf.printf "Prop: %s\n%!" (if r then "true" else "false");
        r
      with e ->
        Printf.printf "Prop: false\n%!";
        raise e
    )

;;
QCheck_base_runner.run_tests_main [
    (*seq_agree_test;*)
    OCSTM_seq.agree_test     ~count:1000 ~name:"STM Out_channel test sequential";
    OCSTM_dom.agree_test_par ~count:1000 ~name:"STM Out_channel test parallel";
  ]
