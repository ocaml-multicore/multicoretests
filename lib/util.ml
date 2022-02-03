(* Repeat a non-determistic property                          *)
(* This is handy if the outcome depends on, e.g., scheduling. *)
let rec repeat n prop = fun input ->
  if n<0 then failwith "repeat: negative repetition count";
  if n=0
  then true
  else prop input && repeat (n-1) prop input

let set_ci_printing () =
  if (Array.mem "--no-colors" Sys.argv)
  && (Array.mem "--verbose" Sys.argv || Array.mem "-v" Sys.argv)
  then
    QCheck_base_runner.set_time_between_msg 2.5

exception Timeout

(* Test a property with a timeout.                       *)
(* This is handy if the tested code can loop infinitely. *)
let prop_timeout sec p x =
  Sys.(signal sigalrm (Signal_handle (fun _ -> raise Timeout))) |> ignore;
  ignore (Unix.alarm sec);
  let res = p x in
  ignore (Unix.alarm 0); (*cancel alarm*)
  res


(* Test a property in a separate process - with a timeout.           *)
(* This is handy if the tested code can segfault or loop infinitely. *)
let fork_prop_with_timeout sec p x =
  let a = Unix.fork () in
  match a with
  | 0  ->
    let _ = Unix.alarm sec in
    if p x
    then (ignore (Unix.alarm 0); exit 0) (*cancel alarm*)
    else (ignore (Unix.alarm 0); exit 2) (*cancel alarm*)
  | _  ->
    let _childid, retcode = Unix.wait () in
    (match retcode with
     | WEXITED code -> (0=code)
     | WSIGNALED _
     | WSTOPPED _  -> false)
