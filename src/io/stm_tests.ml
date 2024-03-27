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
    | Set_binary_mode of bool
    | Set_buffered of bool
    | Is_buffered

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
    | Set_binary_mode b -> cst1 pp_bool "Set_binary_mode" par fmt b
    | Set_buffered b -> cst1 pp_bool "Set_buffered" par fmt b
    | Is_buffered -> cst0 "Is_buffered" fmt

  let show_cmd = Util.Pp.to_show pp_cmd

  (* a path and an open channel to that file; we need to keep the path
     to cleanup after the test run *)
  type sut = { path            : string;
               mutable channel : Out_channel.t }

  type state = Closed
             | Open of { position    : int64;
                         length      : int64;
                         buffered    : bool;
                         binary_mode : bool; }

  let arb_cmd s =
    let int64_gen = Gen.(map Int64.of_int small_int) in
    let char_gen = Gen.printable in
    let byte_gen = Gen.small_int in
    let string_gen = Gen.small_string in
    let bytes_gen = Gen.bytes_small in
    QCheck.make ~print:show_cmd (*~shrink:shrink_cmd*)
      (match s with
       | Closed ->
         Gen.(frequency [ (* generate only Open or Close cmds in Closed *)
             20,return Open_text;
             1,map (fun i -> Seek i) int64_gen;
             1,return Pos;
             1,return Length;
             1,return Close;
             1,return Close_noerr;
             1,return Flush;
             1,map (fun c -> Output_char c) char_gen;
             1,map (fun i -> Output_byte i) byte_gen;
             1,map (fun c -> Output_string c) string_gen;
             1,map (fun b -> Output_bytes b) bytes_gen;
             1,map3 (fun b p l -> Output (b,p,l)) bytes_gen byte_gen byte_gen;
             1,map3 (fun s p l -> Output_substring (s,p,l)) string_gen byte_gen byte_gen;
             1,map (fun b -> Set_binary_mode b) Gen.bool;
             1,map (fun b -> Set_buffered b) Gen.bool;
             1,return Is_buffered;
           ])
       | Open _ ->
         Gen.(frequency [
             (*1,return Open_text;*)
             3,map (fun i -> Seek i) int64_gen;
             3,return Pos;
             3,return Length;
             1,return Close;
             1,return Close_noerr;
             3,return Flush;
             3,map (fun c -> Output_char c) char_gen;
             3,map (fun i -> Output_byte i) byte_gen;
             3,map (fun c -> Output_string c) string_gen;
             3,map (fun b -> Output_bytes b) bytes_gen;
             3,map3 (fun b p l -> Output (b,p,l)) bytes_gen byte_gen byte_gen;
             3,map3 (fun s p l -> Output_substring (s,p,l)) string_gen byte_gen byte_gen;
             3,map (fun b -> Set_binary_mode b) Gen.bool;
             3,map (fun b -> Set_buffered b) Gen.bool;
             3,return Is_buffered;
           ]))

  let init_state  = Open { position = 0L;
                           length = 0L;
                           buffered = true;
                           binary_mode = false; }

  let count_nls s =
    String.fold_right (fun c count -> if c = '\n' then 1+count else count) s 0

  let in_windows_text_mode binary_mode =
    (Sys.win32 || Sys.cygwin) && not binary_mode

  let next_state c s = match c,s with
    | Open_text, Closed ->
      Open { position = 0L;
             length = 0L;
             buffered = true;
             binary_mode = false; }
    | Open_text, Open _ -> s
    (* non-open cmd on closed Out_channel *)
    | Output_char _, Closed
    | Output_byte _, Closed
    | Output_string _, Closed
    | Output_bytes _, Closed
    | Output (_,_,_), Closed
    | Output_substring (_,_,_), Closed
    | Seek _, Closed
    | Close, Closed
    | Close_noerr, Closed
    | Set_binary_mode _, Closed
    | Set_buffered _, Closed -> s
    (* non-open cmd on open Out_channel *)
    | Seek p, Open { position = _; length; buffered; binary_mode } ->
      Open { position = p;
             length = Int64.max length p;
             buffered;
             binary_mode; }
    | Pos,_ -> s
    | Length,_ -> s
    | Close, Open _ -> Closed
    | Close_noerr, Open _ -> Closed
    | Flush, _ -> s
    | Set_binary_mode b, Open { position; length; buffered; binary_mode = _ } ->
      Open { position; length; buffered; binary_mode = b }
    | Is_buffered, _ -> s
    | Set_buffered b, Open { position; length; buffered = _; binary_mode } ->
      Open { position; length; buffered = b; binary_mode }
    (* output on open Out_channel *)
    | Output_char c, Open { position; length; buffered; binary_mode } ->
      let position = Int64.succ position in
      let length = (* Windows text mode maps '\n' to "\r\n" *)
        Int64.add length
          (if in_windows_text_mode binary_mode && c = '\n' then 2L else 1L) in
      Open { position; length; buffered; binary_mode; }
    | Output_byte i, Open { position; length; buffered; binary_mode } ->
      let position = Int64.succ position in
      let length = (* Windows text mode maps '\n' to "\r\n" *)
        Int64.add length
          (if in_windows_text_mode binary_mode && (i mod 256 = 10) then 2L else 1L) in
      Open { position; length; buffered; binary_mode; }
    | Output_string arg, Open { position; length; buffered; binary_mode } ->
      let arg_len = String.length arg in
      let position = Int64.add position (Int64.of_int arg_len) in
      let length = (* Windows text mode maps '\n' to "\r\n" *)
        Int64.add length
          (if in_windows_text_mode binary_mode
           then Int64.of_int (arg_len + count_nls arg)
           else Int64.of_int arg_len) in
      Open { position; length; buffered; binary_mode; }
    | Output_bytes arg, Open { position; length; buffered; binary_mode } ->
      let arg_len = Bytes.length arg in
      let position = Int64.add position (Int64.of_int arg_len) in
      let length = (* Windows text mode maps '\n' to "\r\n" *)
        Int64.add length
          (if in_windows_text_mode binary_mode
           then Int64.of_int (arg_len + count_nls (String.of_bytes arg))
           else Int64.of_int arg_len) in
      Open { position; length; buffered; binary_mode; }
    | Output (b,p,l), Open { position; length; buffered; binary_mode } ->
      let bytes_len = Bytes.length b in
      if p < 0 || p >= bytes_len || l < 0 || p+l > bytes_len
      then s
      else
        let position = Int64.add position (Int64.of_int l) in
        let length = (* Windows text mode maps '\n' to "\r\n" *)
          Int64.add length
            (if in_windows_text_mode binary_mode
             then Int64.of_int (l + count_nls String.(sub (of_bytes b) p l))
             else Int64.of_int l) in
        Open { position; length; buffered; binary_mode; }
    | Output_substring (str,p,l), Open { position; length; buffered; binary_mode } ->
      let str_len = String.length str in
      if p < 0 || p >= str_len || l < 0 || p+l > str_len
      then s
      else
        let position = Int64.add position (Int64.of_int l) in
        let length = (* Windows text mode maps '\n' to "\r\n" *)
          Int64.add length
            (if in_windows_text_mode binary_mode
             then Int64.of_int (l + count_nls String.(sub str p l))
             else Int64.of_int l) in
        Open { position; length; buffered; binary_mode; }

  let init_sut () =
    let path = Filename.temp_file "stm-" "" in
    let channel = Out_channel.open_text path in
    { path; channel }

  let cleanup { path; channel } =
    (try Out_channel.close channel with Sys_error _ -> ());
    Sys.remove path

  let precond c s = match c,s with
    | Open_text, Closed -> true
    | Open_text, Open _ -> false
    | _, Closed -> true
    | _, Open _ -> true

  let run c ({path;channel = oc} as r) = match c with
    | Open_text       -> Res (result unit exn, protect (fun path -> (r.channel <- Out_channel.open_text path;())) path)
    | Seek p          -> Res (result unit exn, protect (Out_channel.seek oc) p)
    | Pos             -> Res (result int64 exn, protect Out_channel.pos oc)
    | Length          -> Res (result int64 exn, protect Out_channel.length oc)
    | Close           -> Res (result unit exn, protect Out_channel.close oc)
    | Close_noerr     -> Res (result unit exn, protect Out_channel.close_noerr oc)
    | Flush           -> Res (result unit exn, protect Out_channel.flush oc)
    | Output_char c   -> Res (result unit exn, protect (Out_channel.output_char oc) c)
    | Output_byte i   -> Res (result unit exn, protect (Out_channel.output_byte oc) i)
    | Output_string s -> Res (result unit exn, protect (Out_channel.output_string oc) s)
    | Output_bytes b  -> Res (result unit exn, protect (Out_channel.output_bytes oc) b)
    | Output (b,p,l)  -> Res (result unit exn, protect (Out_channel.output oc b p) l)
    | Output_substring (s,p,l) -> Res (result unit exn, protect (Out_channel.output_substring oc s p) l)
    | Set_binary_mode b ->
      if Sys.win32 || Sys.cygwin
      then Res (result unit exn, protect (fun b -> (Out_channel.flush oc; Out_channel.set_binary_mode oc b)) b) (* flush before changing mode *)
      else Res (result unit exn, protect (Out_channel.set_binary_mode oc) b)
    | Set_buffered b  -> Res (result unit exn, protect (Out_channel.set_buffered oc) b)
    | Is_buffered     -> Res (result bool exn, protect Out_channel.is_buffered oc)

  let postcond c (s:state) res = match c, res with
    | Open_text, Res ((Result (Unit,Exn),_), r) ->
      (match s,r with
       | Closed, (Ok () | Error (Sys_error _)) -> true (*"/tmp/stm-03ba23: Too many open files"*)
       | _ -> false)
    | Seek _, Res ((Result (Unit,Exn),_), r) ->
      (match s,r with
       | Closed, Error (Sys_error _) -> true (* Sys_error("Bad file descriptor") *)
       | Open _, Ok () -> true
       | _ -> false)
    | Pos, Res ((Result (Int64,Exn),_), r) ->
      (match s, r with
       | Closed, (Ok _ | Error (Sys_error _)) -> true (* pos on closed channel unspecified *)
       | Open { position;
                length = _;
                buffered = _;
                binary_mode = _ }, Ok p -> p = position
       | _ -> false)
    | Length, Res ((Result (Int64,Exn),_), r) ->
      (match s,r with
       | Closed, Error (Sys_error _) -> true (* Sys_error("Bad file descriptor") *)
       | Open { position = _;
                length;
                buffered = _;  (* because of buffering the result and the model may disagree *)
                binary_mode = _ }, Ok i -> i <= length
       | _ -> false)
    | Close, Res ((Result (Unit,Exn),_), r) ->
      (match s,r with
       | Closed, (Ok () | Error (Sys_error _)) (*"Close exception" - unspecified *)
       | Open _, Ok () -> true
       | _ -> false)
    | Close_noerr, Res ((Result (Unit,Exn),_), r) ->
      (match s,r with
       | Closed, Error (Sys_error _) -> false (* should not generate an error *)
       | Closed, Ok () -> true
       | Open _, Ok () -> true
       | _ -> false)
    | Flush, Res ((Result (Unit,Exn),_), r) ->
      (match s,r with
       | Closed, Error (Sys_error _) -> false (* should not generate an error *)
       | Closed, Ok () -> true
       | Open _, Ok () -> true
       | _ -> false)
    | Output_char _c, Res ((Result (Unit,Exn),_), r) ->
      (match s,r with (* "Output functions raise a Sys_error exception when [...] applied to a closed output channel" *)
       | Closed, Error (Sys_error _) -> true
       | Open _, Ok () -> true
       | _ -> false)
    | Output_byte _i, Res ((Result (Unit,Exn),_), r) ->
      (match s,r with (* "Output functions raise a Sys_error exception when [...] applied to a closed output channel" *)
       | Closed, Error (Sys_error _) -> true
       | Open _, Ok () -> true
       | _ -> false)
    | Output_string str, Res ((Result (Unit,Exn),_), r) ->
      (match s,r with (* "Output functions raise a Sys_error exception when [...] applied to a closed output channel" *)
       | Closed, Error (Sys_error _) -> true
       | Closed, Ok () -> str = "" (* accepting this is actually against the above spec *)
       | Open _, Ok () -> true
       | _ -> false)
    | Output_bytes b, Res ((Result (Unit,Exn),_), r) ->
      (match s,r with (* "Output functions raise a Sys_error exception when [...] applied to a closed output channel" *)
       | Closed, Error (Sys_error _) -> true
       | Closed, Ok () -> b = Bytes.empty (* accepting this is actually against the above spec *)
       | Open _, Ok () -> true
       | _ -> false)
    | Output (b,p,l), Res ((Result (Unit,Exn),_), r) ->
      (match s,r with (* "Output functions raise a Sys_error exception when [...] applied to a closed output channel" *)
       | Closed, Error (Sys_error _) -> true
       | Closed, Ok () -> l = 0 (* accepting this is actually against the above spec *)
       | Open _, Ok () -> true
       | (Open _|Closed), Error (Invalid_argument _) -> (*"output"*)
         let bytes_len = Bytes.length b in
         p < 0 || p >= bytes_len || l < 0 || p+l > bytes_len
       | _, _ -> false)
    | Output_substring (str,p,l), Res ((Result (Unit,Exn),_), r) ->
      (match s,r with (* "Output functions raise a Sys_error exception when [...] applied to a closed output channel" *)
       | Closed, Error (Sys_error _) -> true
       | Closed, Ok () -> l = 0 (* accepting this is actually against the above spec *)
       | Open _, Ok () -> true
       | (Open _|Closed), Error (Invalid_argument _) -> (*"output_substring"*)
         let str_len = String.length str in
         p < 0 || p >= str_len || l < 0 || p+l > str_len
       | _, _ -> false)
    | Set_binary_mode _, Res ((Result (Unit,Exn),_), r) ->
      (match s,r with
       | Closed, (Ok () | Error (Sys_error _)) -> true (* set_binary_mode on closed channel unspecified *)
       | Open _, Ok () -> true
       | _, _ -> false)
    | Set_buffered _, Res ((Result (Unit,Exn),_), r) ->
      (match s,r with
       | Closed, (Ok () | Error (Sys_error _)) -> true (* set_buffered on closed channel unspecified *)
       | Open _, Ok () -> true
       | _, _ -> false)
    | Is_buffered, Res ((Result (Bool,Exn),_), r) ->
      (match s,r with
       | Closed, (Ok _ | Error (Sys_error _)) -> true (* is_buffered on closed channel unspecified *)
       | Open { position = _;
                length = _;
                buffered;
                binary_mode = _ }, Ok r -> r = buffered
       | _, _ -> false)
    | _, _ -> false
end

module OCSTM_seq = STM_sequential.Make(OCConf)
module OCSTM_dom = STM_domain.Make(OCConf)
;;
QCheck_base_runner.run_tests_main [
  OCSTM_seq.agree_test     ~count:1000 ~name:"STM Out_channel test sequential";
  OCSTM_dom.agree_test_par ~count:1000 ~name:"STM Out_channel test parallel";
]
