(* Custom runner for the tests so that:
   - error codes on Windows are turned back into their Unix meaninrgs
   - anchors are added to CI logs with relevant information *)

let use_github_anchors = Sys.getenv_opt "CI" = Some "true"

let signals =
  let open Sys in
  [
    (sigabrt, "ABRT");
    (sigalrm, "ALRM");
    (sigfpe, "FPE");
    (sighup, "HUP");
    (sigill, "ILL");
    (sigint, "INT");
    (sigkill, "KILL");
    (sigpipe, "PIPE");
    (sigquit, "QUIT");
    (sigsegv, "SEGV");
    (sigterm, "TERM");
    (sigusr1, "USR1");
    (sigusr2, "USR2");
    (sigchld, "CHLD");
    (sigcont, "CONT");
    (sigstop, "STOP");
    (sigtstp, "TSTP");
    (sigttin, "TTIN");
    (sigttou, "TTOU");
    (sigvtalrm, "VTALRM");
    (sigprof, "PROF");
    (sigbus, "BUS");
    (sigpoll, "POLL");
    (sigsys, "SYS");
    (sigtrap, "TRAP");
    (sigurg, "URG");
    (sigxcpu, "XCPU");
    (sigxfsz, "XFSZ");
  ]

let error fmt cmd msg =
  if use_github_anchors then
    Format.fprintf fmt "\n::error title=%s in %s::%s in %s\n%!" msg cmd msg cmd
  else Format.fprintf fmt "\nError: %s in %s\n%!" msg cmd

let warning fmt cmd msg =
  if use_github_anchors then
    Format.fprintf fmt "\n::warning title=%s in %s::%s in %s\n%!" msg cmd msg
      cmd
  else Format.fprintf fmt "\nWarning: %s in %s\n%!" msg cmd

let timed_out = Atomic.make false

let pp_status_unix fmt cmd status =
  let open Unix in
  let success = ref false in
  (match status with
  | WEXITED 0 -> success := true
  | WEXITED s -> error fmt cmd (Printf.sprintf "Exit %d" s)
  | WSIGNALED s when Atomic.get timed_out && (s = Sys.sigkill || s = Sys.sigterm)
    ->
      warning fmt cmd "Deadline reached, test interrupted";
      (* We nevertheless want the test to globally succeed *)
      success := true
  | WSIGNALED s ->
      let msg =
        match List.assoc_opt s signals with
        | Some signal -> "Signal " ^ signal
        | None -> Printf.sprintf "Unknown signal %d" s
      in
      error fmt cmd msg
  | WSTOPPED s ->
      let msg =
        match List.assoc_opt s signals with
        | Some signal -> "Stop with signal " ^ signal
        | None -> Printf.sprintf "Stop with unknown signal %d" s
      in
      error fmt cmd msg);
  !success

(* Under Windows, there is no such thing as terminating due to a
   signal, so the WSIGNALED and WSTOPPED cases are dead code.

   The strategy is to use conventional exit values (which are 32-bit,
   not just 8-bit like on Unix) to describe the cause.
   The documentation of ”NTSTATUS Values” list {e many} cases, too
   many to handle them all. This is where the value akin to SEGV comes
   from. Other special cases will be caught as they appear.

   The value used to match ABRT comes from the code of the abort
   function in the standard library.

   {{:https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-erref/596a1078-e883-4972-9bbc-49e60bebca55}NTSTATUS Values}
*)
let pp_status_win fmt cmd status =
  let open Unix in
  (match status with
  | WEXITED 0 -> ()
  | WEXITED 3 -> error fmt cmd "Signal ABRT"
  | WEXITED -1073741819 (* 0xC0000005 *) -> error fmt cmd "Signal SEGV"
  | WEXITED s -> error fmt cmd (Printf.sprintf "Exit %d" s)
  (* Those last 2 cases are dead code on Windows *)
  | WSIGNALED s ->
      let msg =
        match List.assoc_opt s signals with
        | Some signal -> "Signal " ^ signal
        | None -> Printf.sprintf "Unknown signal %d" s
      in
      error fmt cmd msg
  | WSTOPPED s ->
      let msg =
        match List.assoc_opt s signals with
        | Some signal -> "Stop with signal " ^ signal
        | None -> Printf.sprintf "Stop with unknown signal %d" s
      in
      error fmt cmd msg);
  status = WEXITED 0

let pp_status = if Sys.win32 then pp_status_win else pp_status_unix
let start_time = Unix.time ()

let deadline =
  let getfloat v = Option.bind (Sys.getenv_opt v) float_of_string_opt in
  let global = Option.value ~default:Float.infinity (getfloat "DEADLINE") in
  match getfloat "TEST_TIMEOUT" with
  | None -> global
  | Some t -> min global (start_time +. (t *. 60.))

let deadline_watcher pid () =
  let open Unix in
  assert (deadline > start_time);
  if Float.is_finite deadline then (
    sleepf (deadline -. start_time);
    Atomic.set timed_out true;
    if not Sys.win32 then (
      (* let's give it a little time to stop *)
      kill pid Sys.sigterm;
      sleep 2);
    kill pid Sys.sigkill)

let log_time cmd =
  match Sys.getenv_opt "TIMELOGDIR" with
  | None -> ()
  | Some d ->
      let f = Filename.concat d "times.log" in
      let flags = [ Open_wronly; Open_append; Open_creat; Open_binary ] in
      Out_channel.with_open_gen flags 0o666 f @@ fun oc ->
      let dur = int_of_float (Unix.time () -. start_time) in
      let hours = dur / 3600
      and minutes = dur mod 3600 / 60
      and seconds = dur mod 60 in
      if hours > 0 then
        Printf.fprintf oc "%-40s finished in %d:%02d:%02d (%ds)\n" cmd hours
          minutes seconds dur
      else
        Printf.fprintf oc "%-40s finished in   %02d:%02d (%ds)\n" cmd minutes
          seconds dur

let run ofmt efmt argv =
  let argv =
    match argv with [| cmd |] -> [| cmd; "--verbose" |] | _ -> argv
  in
  let testdir = Filename.basename (Sys.getcwd ()) in
  let exe, cmd =
    if Filename.is_implicit argv.(0) then
      ( Filename.concat Filename.current_dir_name argv.(0),
        Filename.concat testdir argv.(0) )
    else (argv.(0), argv.(0))
  in
  let cmdline = String.concat " " (Array.to_list argv) in
  if start_time < deadline then (
    Format.fprintf ofmt "\n\nStarting (in %s) %s:\n%!" testdir cmdline;
    let pid = Unix.(create_process exe argv stdin stdout stderr) in
    ignore @@ Domain.spawn (deadline_watcher pid);
    let _, status = Unix.waitpid [] pid in
    log_time cmd;
    pp_status efmt cmd status)
  else (
    warning ofmt cmd "Deadline reached, skipping test";
    true)

let _ =
  let open Format in
  if Array.length Sys.argv < 2 then (
    fprintf err_formatter
      "\nError: %s expects the\n  command to run as argument\n%!" Sys.argv.(0);
    exit 1);
  let cmd = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  let success = run std_formatter err_formatter cmd in
  if not success then exit 1
