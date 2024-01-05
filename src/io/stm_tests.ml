open QCheck
open STM

(** parallel STM tests of Weak arrays *)

module OCConf =
struct
  type cmd =
    | Open_text
    (*| Seek of int64*)
    | Pos
    | Length
    | Close
    | Flush
    | Output_char of char
    | Output_string of string

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Open_text -> cst0 "Open_text" fmt
    (*| Seek i -> cst1 pp_int64 "Seek" par fmt i*)
    | Pos -> cst0 "Pos" fmt
    | Length -> cst0 "Length" fmt
    | Close -> cst0 "Close" fmt
    | Flush -> cst0 "Flush" fmt
    | Output_char c -> cst1 pp_char "Output_char" par fmt c
    | Output_string s -> cst1 pp_string "Output_string" par fmt s

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
    let _int64_gen = Gen.(map Int64.of_int small_int) in
    let char_gen = Gen.printable in
    let string_gen = Gen.small_string in
    QCheck.make ~print:show_cmd (*~shrink:shrink_cmd*)
      (match s with
       | Closed _ -> Gen.return Open_text
       | Open _ ->
          Gen.(frequency
                 [
                   (*1,return Open_text;*)
                   (*3,map (fun i -> Seek i) int64_gen;*)
                   3,return Pos;
                   3,return Length;
                   1,return Close;
                   3,return Flush;
                   3,map (fun c -> Output_char c) char_gen;
                   3,map (fun c -> Output_string c) string_gen;
          ]))

  let init_state  = Closed 0L (*Open { position = 0L; length = 0L }*)

  let next_state c s = match c,s with
    | Open_text, Closed _l -> Open { position = 0L; length = (*l*) 0L }
    | Open_text, Open _ -> s
(*  | Seek _, Closed _ -> s
    | Seek p, Open _ -> Open { position = p; length = (*l*) 0L }*)
    | Pos,_ -> s
    | Length,_ -> s
    | Close, Open { position = _; length = _ } -> Closed 0L(*length*)
    | Close, Closed _ -> s
    | Flush, Open _ -> s(*length*)
    | Flush, Closed _ -> s
    | Output_char _c, Closed _ -> s
    | Output_char _c, Open { position; length } ->
       Open {position = Int64.succ position;
             length   = Int64.succ length; }
    | Output_string _str, Closed _ -> s
    | Output_string str, Open { position; length } ->
       let str_len = Int64.of_int (String.length str) in
       Open {position = Int64.add position str_len;
             length   = Int64.add length str_len; }

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
    (*| Seek p          -> Res (unit, Out_channel.seek oc p)*)
    | Pos             -> Res (result int64 exn, protect Out_channel.pos oc)
    | Length          -> Res (int64, Out_channel.length oc)
    | Close           -> Res (result unit exn, protect Out_channel.close oc)
    | Flush           -> Res (unit, Out_channel.flush oc)
    | Output_char c   -> Res (result unit exn, protect (Out_channel.output_char oc) c)
    | Output_string s -> Res (result unit exn, protect (Out_channel.output_string oc) s)

  let postcond c (s:state) res = match c, res with
    | Open_text, Res ((Result (Unit,Exn),_), r) ->
       (match s,r with
        | Closed _, Ok ()
        | Closed _, Error (Sys_error _) (*"/tmp/lin-dsl-03ba23: Too many open files"*)
        | Open _, Ok ()
        | Open _, Error (Sys_error _) -> true
        | _ -> false)
  (*| Seek _, Res ((Unit,_), ()) -> true*)
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
       ((*match s with
        | Closed -> r = Error (Invalid_argument "Close exception")
        | Open {position = _} ->*) r = Ok ())
    | Flush, Res ((Unit,_), r) -> r = ()
    | Output_char _c, Res ((Result (Unit,Exn),_), r) ->
       (match s with
        | Closed _ -> true
        | Open _ -> r = Ok ()) (* print on closed unspecified *)
   (* if s = Closed
      then r = Error (Sys_error "Bad file descriptor")
      else r = Ok () *)
    | Output_string _c, Res ((Result (Unit,Exn),_), r) ->
       (match s with
        | Closed _ -> true
        | Open _ -> r = Ok ()) (* print on closed unspecified *)
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
    OCSTM_seq.agree_test         ~count:1000 ~name:"STM Out_channel test sequential";
    OCSTM_dom.agree_test_par     ~count:1000 ~name:"STM Out_channel test parallel";
  ]
