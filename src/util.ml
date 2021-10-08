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
