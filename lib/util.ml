let rec repeat n prop = fun input ->
  if n<0 then failwith "repeat: negative repetition count";
  if n=0
  then true
  else prop input && repeat (n-1) prop input

exception Timeout

let prop_timeout sec p x =
  Sys.(signal sigalrm (Signal_handle (fun _ -> raise Timeout))) |> ignore;
  ignore (Unix.alarm sec);
  let res = p x in
  ignore (Unix.alarm 0); (*cancel alarm*)
  res

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
     | WSIGNALED s when s = Sys.sigalrm -> raise Timeout
     | WSIGNALED _
     | WSTOPPED _  -> false)

let print_vertical ?(fig_indent=3) show cmds =
  let cmds = List.map show cmds in
  let buf = Buffer.create 64 in
  let indent () = Printf.bprintf buf "%s" (String.make fig_indent ' ') in
  let print_seq_col c = Printf.bprintf buf "%s\n" c in
  let () = List.iter (fun c -> indent (); print_seq_col c) cmds in
  Buffer.contents buf

let print_triple_vertical ?(fig_indent=10) ?(res_width=20) ?(center_prefix=true) show (seq,cmds1,cmds2) =
  let seq,cmds1,cmds2 = List.(map show seq, map show cmds1, map show cmds2) in
  let max_width ss = List.fold_left max 0 (List.map String.length ss) in
  let width = List.fold_left max 0 [max_width seq; max_width cmds1; max_width cmds2] in
  let res_width = max width res_width in
  let cmd_indent = String.make ((width-1)/2) ' ' in
  let seq_indent = String.make ((res_width + 3)/2) ' ' in
  let bar_cmd = Printf.sprintf "%-*s" res_width (cmd_indent ^ "|") in
  let center c =
    let clen = String.length c in
    if clen > width (* it's a '|'-string *)
    then c
    else Printf.sprintf "%s%s" (String.make ((width - clen)/2) ' ') c in
  let buf = Buffer.create 64 in
  let indent () = Printf.bprintf buf "%s" (String.make fig_indent ' ') in
  let print_seq_col c = Printf.bprintf buf "%s%-*s\n" seq_indent res_width c in
  let print_par_col c1 c2 = Printf.bprintf buf "%-*s  %-*s\n" res_width c1 res_width c2 in
  let print_hoz_line () =
    Printf.bprintf buf "%-*s\n" res_width (cmd_indent ^ "." ^ (String.make (res_width + 1) '-') ^ ".") in
  let rec print_par_cols cs cs' = match cs,cs' with
    | [],   []    -> ()
    | c::cs,[]    -> indent (); print_par_col (center c) ""; print_par_cols cs []
    | [],   c::cs -> indent (); print_par_col "" (center c); print_par_cols [] cs
    | l::ls,r::rs -> indent (); print_par_col (center l) (center r); print_par_cols ls rs in
  (* actual printing *)
  if center_prefix
  then
    List.iter (fun c -> indent (); print_seq_col (center c)) ([bar_cmd] @ seq @ [bar_cmd])
  else
    List.iter (fun c -> indent (); print_par_col (center c) "") (bar_cmd::seq@[bar_cmd]);
  indent (); print_hoz_line ();
  print_par_cols (bar_cmd::cmds1) (bar_cmd::cmds2);
  Buffer.contents buf

let protect (f : 'a -> 'b) (a : 'a) : ('b, exn) result =
  try Result.Ok (f a)
  with e -> Result.Error e

module Pp = struct
  open Format

  type 'a t = bool -> Format.formatter -> 'a -> unit

  let to_show f x = asprintf "%a" (f false) x

  let of_show f par fmt x =
    fprintf fmt (if par then "(%s)" else "%s") (f x)

  let cst0 name fmt = pp_print_string fmt name

  let cst1 (pp : 'a t) name par fmt x =
    fprintf fmt (if par then "(%s %a)" else "%s %a") name (pp true) x

  let cst2 (pp1 : 'a t) (pp2 : 'b t) name par fmt x y =
    fprintf fmt (if par then "(%s (%a, %a))" else "%s (%a, %a)") name (pp1 false) x (pp2 false) y

  let cst3 (pp1 : 'a t) (pp2 : 'b t) (pp3 : 'c t) name par fmt x y z =
    fprintf fmt
      (if par then "(%s (%a, %a, %a))" else "%s (%a, %a, %a)")
      name (pp1 false) x (pp2 false) y (pp3 false) z

  let pp_exn = of_show Printexc.to_string
  let pp_unit _ fmt () = pp_print_string fmt "()"
  let pp_bool _ fmt b = fprintf fmt "%B" b
  let pp_int par fmt i = fprintf fmt (if par && i < 0 then "(%d)" else "%d") i
  let pp_int64 par fmt i = fprintf fmt (if par && i < 0L then "(%LdL)" else "%LdL") i
  let pp_float par fmt f = fprintf fmt (if par && f < 0.0 then "(%F)" else "%F") f
  let pp_char _ fmt c = fprintf fmt "%C" c
  let pp_string _ fmt s = fprintf fmt "%S" s
  let pp_bytes _ fmt s = fprintf fmt "%S" (Bytes.to_string s)

  let pp_option (pp_s : 'a t) par fmt o =
    match o with
    | None -> pp_print_string fmt "None"
    | Some s -> fprintf fmt (if par then "(Some %a)" else "Some %a") (pp_s true) s

  let pp_result (pp_o : 'o t) (pp_e : 'e t) par fmt r =
    let open Result in
    match r with
    | Ok o -> fprintf fmt (if par then "(Ok %a)" else "Ok %a") (pp_o true) o
    | Error e -> fprintf fmt (if par then "(Error %a)" else "Error %a") (pp_e true) e

  let pp_pair (pp_f : 'a t) (pp_s : 'b t) _ fmt (x,y) =
    fprintf fmt "(%a, %a)" (pp_f false) x (pp_s false) y

  let pp_list (pp_e : 'a t) _ fmt l =
    pp_print_string fmt "[";
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") (pp_e false) fmt l;
    pp_print_string fmt "]"

  let pp_seq (pp_e : 'a t) _ fmt s =
    pp_print_string fmt "<";
    pp_print_seq ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") (pp_e false) fmt s;
    pp_print_string fmt ">"

  let pp_array (pp_e : 'a t) _ fmt a =
    pp_print_string fmt "[|";
    pp_print_seq ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") (pp_e false) fmt (Array.to_seq a);
    pp_print_string fmt "|]"

  type pp_field = Format.formatter -> unit

  let pp_field name (pp_c : 'a t) c fmt =
    fprintf fmt "%s =@ %a" name (pp_c false) c

  let pp_record _ fmt fields =
    pp_print_string fmt "{ ";
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") (fun fmt ppf -> ppf fmt) fmt fields;
    fprintf fmt "@ }"
end

module Equal = struct
  type 'a t = 'a -> 'a -> bool

  let equal_exn = ( = )
  let equal_unit = Unit.equal
  let equal_bool = Bool.equal
  let equal_int = Int.equal
  let equal_int64 = Int64.equal
  let equal_float = Float.equal
  let equal_char = Char.equal
  let equal_string = String.equal
  let equal_option = Option.equal
  let equal_result eq_o eq_e x y = Result.equal ~ok:eq_o ~error:eq_e x y
  let equal_list = List.equal
  let rec equal_seq eq s1 s2 = (* To support OCaml 4.13 as Seq.equal was added in 4.14 *)
    let open Seq in
    match s1 (), s2 () with
    | Nil, Nil -> true
    | Cons (a, an), Cons (b, bn) when eq a b -> equal_seq eq an bn
    | _ -> false
  let equal_array eq x y = equal_seq eq (Array.to_seq x) (Array.to_seq y)
end

module QCheck_base_runner = struct
  (*open QCheck_base_runner*)

  module Color = QCheck_base_runner.Color

  let seed = ref ~-1
  let st = ref None

  let set_seed_ ~colors s =
    seed := s;
    if colors then Printf.printf "%srandom seed: %d\n%!" Color.reset_line s
    else Printf.printf "random seed: %d\n%!" s;
    let state = Random.State.make [| s |] in
    st := Some state;
    state

  (* time of last printed message. Useful for rate limiting in verbose mode *)
  let last_msg = ref 0.

  let time_between_msg =
    let env_var = "QCHECK_MSG_INTERVAL" in
    let default_interval = 0.1 in
    let interval = match Sys.getenv_opt env_var with
      | None -> default_interval
      | Some f ->
        match float_of_string_opt f with
        | None -> invalid_arg (env_var ^ " must be a float")
        | Some i -> i in
    if interval < 0. then invalid_arg (env_var ^ " must be >= 0 but value is " ^ string_of_float interval);
    ref interval

  let get_time_between_msg () = !time_between_msg

(*let set_time_between_msg f = time_between_msg := f*)

(*let set_seed s = ignore (set_seed_ ~colors:false s)*)

  let setup_random_state_ ~colors () =
    let s = if !seed = ~-1 then (
        Random.self_init ();  (* make new, truly random seed *)
        Random.int (1 lsl 29);
      ) else !seed in
    set_seed_ ~colors s

  (* initialize random generator from seed (if any) *)
  let random_state_ ~colors () = match !st with
    | Some st -> st
    | None -> setup_random_state_ ~colors ()

(*let random_state() = random_state_ ~colors:false ()*)

  let verbose, _set_verbose =
    let r = ref false in
    (fun () -> !r), (fun b -> r := b)

  let long_tests, _set_long_tests =
    let r = ref false in
    (fun () -> !r), (fun b -> r := b)

  let debug_shrink, _set_debug_shrink =
    let r = ref None in
    (fun () -> !r), (fun s -> r := Some (open_out s))

  let debug_shrink_list, _set_debug_shrink_list =
    let r = ref [] in
    (fun () -> !r), (fun b -> r := b :: !r)

  (* ... *)

  type counter = {
    start : float;
    expected : int;
    mutable gen : int;
    mutable passed : int;
    mutable failed : int;
    mutable errored : int;
  }

  type res =
    | Res : 'a QCheck2.Test.cell * 'a QCheck2.TestResult.t -> res

  type handler = {
    handler : 'a. 'a QCheck2.Test.handler;
  }

  type handler_gen =
    colors:bool ->
    debug_shrink:(out_channel option) ->
    debug_shrink_list:(string list) ->
    size:int -> out:out_channel -> verbose:bool -> counter -> handler

  let pp_counter ~size out c =
    let t = Unix.gettimeofday () -. c.start in
    Printf.fprintf out "%*d %*d %*d %*d / %*d %7.1fs"
      size c.gen size c.errored size c.failed
      size c.passed size c.expected t

  let debug_shrinking_counter_example cell out x =
    match QCheck2.Test.get_print_opt cell  with
    | None -> Printf.fprintf out "<no printer provided>"
    | Some print -> Printf.fprintf out "%s" (print x)

  let debug_shrinking_choices ~colors ~out ~name cell ~step x =
    Printf.fprintf out "\n~~~ %a %s\n\n"
      (Color.pp_str_c ~colors `Cyan) "Shrink" (String.make 69 '~');
    Printf.fprintf out
      "Test %s successfully shrunk counter example (step %d) to:\n\n%a\n%!"
      name step
      (debug_shrinking_counter_example cell) x

  let default_handler
      ~colors ~debug_shrink ~debug_shrink_list
      ~size ~out ~verbose c =
    let handler name cell r =
      let st = function
        | QCheck2.Test.Generating    -> "generating"
        | QCheck2.Test.Collecting _  -> "collecting"
        | QCheck2.Test.Testing _     -> "   testing"
        | QCheck2.Test.Shrunk (i, _) ->
          Printf.sprintf "shrinking: %4d" i
        | QCheck2.Test.Shrinking (i, j, _) ->
          Printf.sprintf "shrinking: %4d.%04d" i j
      in
      (* debug shrinking choices *)
      begin match r with
        | QCheck2.Test.Shrunk (step, x) ->
          begin match debug_shrink with
            | None -> ()
            | Some out ->
              let go =
                match debug_shrink_list with
                | [] -> true
                | test_list -> List.mem name test_list
              in
              if not go then ()
              else
                debug_shrinking_choices
                  ~colors ~out ~name cell ~step x
          end
        | _ ->
          ()
      end;
      (* use timestamps for rate-limiting *)
      let now=Unix.gettimeofday() in
      if verbose && now -. !last_msg > get_time_between_msg () then (
        last_msg := now;
        Printf.fprintf out "%s[ ] %a %s (%s)%!"
          (if colors then Color.reset_line else "\n")
          (pp_counter ~size) c name (st r)
      )
    in
    { handler; }

  let step ~colors ~size ~out ~verbose c name _ _ r =
    let aux = function
      | QCheck2.Test.Success -> c.passed <- c.passed + 1
      | QCheck2.Test.Failure -> c.failed <- c.failed + 1
      | QCheck2.Test.FalseAssumption -> ()
      | QCheck2.Test.Error _ -> c.errored <- c.errored + 1
    in
    c.gen <- c.gen + 1;
    aux r;
    let now=Unix.gettimeofday() in
    if verbose && now -. !last_msg > get_time_between_msg () then (
      last_msg := now;
      Printf.fprintf out "%s[ ] %a %s%!"
        (if colors then Color.reset_line else "\n") (pp_counter ~size) c name
    )

  let callback ~size ~out ~verbose ~colors c name cell r =
    let pass =
      if QCheck2.Test.get_positive cell
      then QCheck2.TestResult.is_success r
      else QCheck2.TestResult.is_failed r in
    let color = if pass then `Green else `Red in
    if verbose then (
      Printf.fprintf out "%s[%a] %a %s\n%!"
        (if colors then Color.reset_line else "\n")
        (Color.pp_str_c ~bold:true ~colors color) (if pass then "✓" else "✗")
        (pp_counter ~size) c name
    )

  (* ... *)

  let print_inst cell x =
    match QCheck2.Test.get_print_opt cell with
    | Some f -> f x
    | None -> "<no printer>"

  let expect long cell =
    let count = QCheck2.Test.get_count cell in
    if long then QCheck2.Test.get_long_factor cell * count else count

  let expect_size long cell =
    let rec aux n = if n < 10 then 1 else 1 + (aux (n / 10)) in
    aux (expect long cell)

  let print_message_list out l =
    if l<>[] then List.iter (Printf.fprintf out "%s\n%!") l

  (* print user messages for a test *)
  let print_messages ~colors out cell l =
    if l<>[] then (
      Printf.fprintf out
        "\n+++ %a %s\n\nMessages for test %s:\n\n%!"
        (Color.pp_str_c ~colors `Blue) "Messages"
        (String.make 68 '+') (QCheck2.Test.get_name cell);
      print_message_list out l
    )

  let print_success ~colors out cell r =
    begin match QCheck2.TestResult.collect r with
      | None -> ()
      | Some tbl ->
        Printf.fprintf out
          "\n+++ %a %s\n\nCollect results for test %s:\n\n%s%!"
          (Color.pp_str_c ~colors `Blue) "Collect"
          (String.make 68 '+') (QCheck2.Test.get_name cell) (QCheck2.Test.print_collect tbl)
    end;
    List.iter (fun msg ->
        Printf.fprintf out
          "\n!!! %a %s\n\nWarning for test %s:\n\n%s%!"
          (Color.pp_str_c ~colors `Yellow) "Warning" (String.make 68 '!')
          (QCheck2.Test.get_name cell) msg)
      (QCheck2.TestResult.warnings r);

    if QCheck2.TestResult.stats r <> []  then
      Printf.fprintf out
        "\n+++ %a %s\n%!"
        (Color.pp_str_c ~colors `Blue) ("Stats for " ^ QCheck2.Test.get_name cell)
        (String.make 56 '+');
    List.iter
      (fun st -> Printf.fprintf out "\n%s%!" (QCheck2.Test.print_stat st))
      (QCheck2.TestResult.stats r);
    ()

  let print_fail ~colors out cell c_ex =
    Printf.fprintf out "\n--- %a %s\n\n" (Color.pp_str_c ~colors `Red) "Failure" (String.make 68 '-');
    Printf.fprintf out "Test %s failed (%d shrink steps):\n\n%s\n%!"
      (QCheck2.Test.get_name cell) c_ex.QCheck2.TestResult.shrink_steps
      (print_inst cell c_ex.QCheck2.TestResult.instance);
    print_messages ~colors out cell c_ex.QCheck2.TestResult.msg_l

  let print_fail_other ~colors out cell msg =
    Printf.fprintf out "\n--- %a %s\n\n" (Color.pp_str_c ~colors `Red) "Failure" (String.make 68 '-');
    Printf.fprintf out "Test %s failed:\n\n%s\n%!" (QCheck2.Test.get_name cell) msg

  let has_test_messages c = c.QCheck2.TestResult.msg_l<>[]

  let print_expected_failure ~colors out cell c_ex =
    Printf.fprintf out "\n--- %a %s\n\n" (Color.pp_str_c ~colors `Blue) "Info" (String.make 71 '-');
    Printf.fprintf out "Negative test %s failed as expected (%d shrink steps):\n\n"
      (QCheck2.Test.get_name cell) c_ex.QCheck2.TestResult.shrink_steps;
    if has_test_messages c_ex
    then
      print_message_list out c_ex.QCheck2.TestResult.msg_l
    else
      Printf.fprintf out "%s\n%!" (print_inst cell c_ex.QCheck2.TestResult.instance)

  let print_error ~colors out cell c_ex exn bt =
    Printf.fprintf out "\n=== %a %s\n\n" (Color.pp_str_c ~colors `Red) "Error" (String.make 70 '=');
    Printf.fprintf out "Test %s errored on (%d shrink steps):\n\n%s\n\nexception %s\n%s\n%!"
      (QCheck2.Test.get_name cell)
      c_ex.QCheck2.TestResult.shrink_steps
      (print_inst cell c_ex.QCheck2.TestResult.instance)
      (Printexc.to_string exn)
      bt;
    print_messages ~colors out cell c_ex.QCheck2.TestResult.msg_l

  let run_tests
      ?(handler=default_handler)
      ?(colors=true) ?(verbose=verbose()) ?(long=long_tests())
      ?(debug_shrink=debug_shrink()) ?(debug_shrink_list=debug_shrink_list())
      ?(out=stdout) ?rand l =
    let rand = match rand with Some x -> x | None -> random_state_ ~colors () in
    let module T = QCheck2.Test in
    let module R = QCheck2.TestResult in
    let pp_color = Color.pp_str_c ~bold:true ~colors in
    let size = List.fold_left (fun acc (T.Test cell) ->
        max acc (expect_size long cell)) 4 l in
    if verbose then
      Printf.fprintf out
        "%*s %*s %*s %*s / %*s     time test name\n%!"
        (size + 4) "generated" size "error"
        size "fail" size "pass" size "total";
    let aux_map (T.Test cell) =
      let rand = Random.State.copy rand in
      let expected = expect long cell in
      let start = Unix.gettimeofday () in
      let c = {
        start; expected; gen = 0;
        passed = 0; failed = 0; errored = 0;
      } in
      if verbose then
        Printf.fprintf out "%s[ ] %a %s%!"
          (if colors then Color.reset_line else "")
          (pp_counter ~size) c (T.get_name cell);
      let r = QCheck2.Test.check_cell ~long ~rand
          ~handler:(handler ~colors ~debug_shrink ~debug_shrink_list
                      ~size ~out ~verbose c).handler
          ~step:(step ~colors ~size ~out ~verbose c)
          ~call:(callback ~size ~out ~verbose ~colors c)
          cell
      in
      Res (cell, r)
    in
    let res = List.map aux_map l in
    let aux_fold (total, fail, error, warns) (Res (cell, r)) =
      let warns = warns + List.length (R.get_warnings r) in
      let acc = match R.get_state r, QCheck2.Test.get_positive cell with
        | R.Success, true ->
          print_success ~colors out cell r;
          (total + 1, fail, error, warns)
        | R.Success, false ->
          let msg = Printf.sprintf "Negative test %s succeeded but was expected to fail" (QCheck2.Test.get_name cell) in
          print_fail_other ~colors out cell msg;
          (total + 1, fail + 1, error, warns)
        | R.Failed {instances=l}, true ->
          List.iter (print_fail ~colors out cell) l;
          (total + 1, fail + 1, error, warns)
        | R.Failed {instances=l}, false ->
          if verbose then List.iter (print_expected_failure ~colors out cell) l;
          (total + 1, fail, error, warns)
        | R.Failed_other {msg}, _ ->  (* Failed_other is also considered a failure *)
          print_fail_other ~colors out cell msg;
          (total + 1, fail + 1, error, warns)
        | R.Error {instance=c_ex; exn; backtrace=bt}, _ -> (* Error is always considered a failure *)
          print_error ~colors out cell c_ex exn bt;
          (total + 1, fail, error + 1, warns)
      in
      acc
    in
    let total, fail, error, warns = List.fold_left aux_fold (0, 0, 0,0) res in
    Printf.fprintf out "%s\n" (String.make 80 '=');
    if warns > 0 then Printf.fprintf out "%d warning(s)\n" warns;
    if fail = 0 && error = 0 then (
      Printf.fprintf out "%a (ran %d tests)\n%!"
        (pp_color `Green) "success" total;
      0
    ) else (
      Printf.fprintf out
        "%a (%d tests failed, %d tests errored, ran %d tests)\n%!"
        (pp_color `Red) "failure" fail error total;
      1
    )

  let run_tests_main ?(argv=Sys.argv) l =
    try
      let cli_args = QCheck_base_runner.Raw.parse_cli ~full_options:false argv in
      exit
        (run_tests l
           ~colors:cli_args.cli_colors
           ~verbose:cli_args.cli_verbose
           ~long:cli_args.cli_long_tests ~out:stdout ~rand:cli_args.cli_rand)
    with
    | Arg.Bad msg -> print_endline msg; exit 1
    | Arg.Help msg -> print_endline msg; exit 0
end
