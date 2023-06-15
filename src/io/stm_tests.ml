open QCheck
open STM

(** parallel STM tests of Weak arrays *)

module OCConf =
struct
  type cmd =
    (*| Seek of int64*)
    | Pos
    (*| Length*)
    | Close
    | Output_char of char
    | Output_string of string

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
 (* | Seek i -> cst1 pp_int64 "Seek" par fmt i*)
    | Pos -> cst0 "Pos" fmt
 (* | Length -> cst0 "Length" fmt *)
    | Close -> cst0 "Close" fmt
    | Output_char c -> cst1 pp_char "Output_char" par fmt c
    | Output_string s -> cst1 pp_string "Output_string" par fmt s

  let show_cmd = Util.Pp.to_show pp_cmd

  (* a path and an open channel to that file; we need to keep the path
     to cleanup after the test run *)
  type sut = string * Out_channel.t

  (*type status = Open | Closed*)
  type state = Closed
             | Open of { position : int64 }

  let arb_cmd _s =
    (*let int64_gen = Gen.(map Int64.of_int small_int) in*)
    let char_gen = Gen.printable in
    let string_gen = Gen.small_string in
    QCheck.make ~print:show_cmd (*~shrink:shrink_cmd*)
      Gen.(frequency
             [ (*1,map (fun i -> Seek i) int64_gen;*)
               5,return Pos;
               (*1,return Length;*)
               1,return Close;
               5,map (fun c -> Output_char c) char_gen;
               5,map (fun c -> Output_string c) string_gen;
             ])

  let init_state  = Open { position = 0L }

  let next_state c s = match c,s with
    | Pos,_ -> s
 (* | Length,_ -> s *)
    | Close, _ -> Closed
    | Output_char _c, Closed -> s
    | Output_char _c, Open {position} -> Open {position = Int64.succ position }
    | Output_string _str, Closed -> s
    | Output_string str, Open {position} -> Open {position = Int64.(add position (of_int (String.length str))) }

  let init_sut () = Filename.open_temp_file "lin-dsl-" ""
  let cleanup (path, chan) =
    Out_channel.close chan ;
    Sys.remove path

  let precond c _s = match c with
    | _ -> true

  let run c (_path,oc) = match c with
    | Pos             -> Res (result int64 exn, protect Out_channel.pos oc)
 (* | Length          -> Res (result int64 exn, protect Out_channel.length oc) *)
    | Close           -> Res (result unit exn, protect Out_channel.close oc)
    | Output_char c   -> Res (result unit exn, protect (Out_channel.output_char oc) c)
    | Output_string s -> Res (result unit exn, protect (Out_channel.output_string oc) s)

  let postcond c (s:state) res = match c, res with
    | Pos, Res ((Result (Int64,Exn),_), r) ->
       (match s with
        | Closed -> true (*r = Error (Invalid_argument "Pos exception") - unspecified *)
        | Open {position} -> r = Ok position)
 (* | Length, Res ((Int,_),i) -> i = List.length s *)
    | Close, Res ((Result (Unit,Exn),_), r) ->
       ((*match s with
        | Closed -> r = Error (Invalid_argument "Close exception")
        | Open {position = _} ->*) r = Ok ())
    | Output_char _c, Res ((Result (Unit,Exn),_), r) ->
      s = Closed || r = Ok () (* print on closed unspecified *)
   (* if s = Closed
      then r = Error (Sys_error "Bad file descriptor")
      else r = Ok () *)
    | Output_string _c, Res ((Result (Unit,Exn),_), r) ->
      s = Closed || r = Ok () (* print on closed unspecified *)
    | _, _ -> false
end

module OCSTM_seq = STM_sequential.Make(OCConf)
module OCSTM_dom = STM_domain.Make(OCConf)
;;
QCheck_base_runner.run_tests_main [
    OCSTM_seq.agree_test         ~count:1000 ~name:"STM Out_channel test sequential";
    OCSTM_dom.agree_test_par     ~count:1000 ~name:"STM Out_channel test parallel";
  ]
