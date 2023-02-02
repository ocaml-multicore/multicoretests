(* First generate a big temporary file with random contents. *)
let temp_readonly = Filename.temp_file "fuzz_stdlib" ""

let temp_readonly_size = 1_000_000

let () =
  Random.self_init ();
  let out = Out_channel.open_bin temp_readonly in
  for _ = 1 to temp_readonly_size do
    Out_channel.output_byte out @@ Random.bits () lsr 22
  done;
  Out_channel.close out

module In_channel_ops = struct
  open Lin.Internal [@@alert "-internal"]

  type t = In_channel.t (* Filename and corresponding channel *)

  type cmd = Close of Var.t | Read of Var.t * int | BlindRead of Var.t * int [@@deriving show { with_path = false }]
(*
  let show_cmd =
    let open Printf in function
    | Close -> "Close"
    | Read l -> sprintf "Read %d" l
    | BlindRead l -> sprintf "BlindRead %d" l
*)
  let gen_cmd gen_var =
    let open QCheck.Gen in
    frequency
      [1, map  (fun v -> None, Close v) gen_var;
       6, map2 (fun v l -> None, Read (v,l)) gen_var small_nat;
       6, map2 (fun v l -> None, BlindRead (v,l)) gen_var small_nat;
      ]

  let shrink_cmd _env _ = QCheck.Iter.empty

  let cmd_uses_var v = function
    | Close i
    | Read (i,_)
    | BlindRead (i,_) -> i=v

  type res =
  | UnitRes of (unit, exn) result
  | ReadRes of (string option, exn) result

  let show_res =
    let open Printf in function
    | UnitRes (Ok ()) -> "()"
    | UnitRes (Error e) -> sprintf "UnitRes exception %s" (Printexc.to_string e)
    | ReadRes (Ok (Some s)) -> sprintf "\"%s\"" s
    | ReadRes (Ok None) -> "None"
    | ReadRes (Error e) -> sprintf "ReadRes exception %s" (Printexc.to_string e)

  let equal_res = (=)

  let init () =
    In_channel.open_bin temp_readonly

  let cleanup chan =
    In_channel.close chan

  let run cmd chan =
    match cmd with
    | None, Read (i,l) ->
        begin try ReadRes (Ok (In_channel.really_input_string chan.(i) l))
        with e -> ReadRes (Error e)
        end
    | None, BlindRead (i,l) ->
        begin try ignore (In_channel.really_input_string chan.(i) l); UnitRes (Ok ())
        with e -> UnitRes (Error e)
        end
    | None, Close i ->
        begin try In_channel.close chan.(i); UnitRes (Ok ())
        with e -> UnitRes (Error e)
        end
    | _, _ -> failwith (Printf.sprintf "unexpected command: %s" (show_cmd (snd cmd)))
end

module Out_channel_ops = struct
  open Lin.Internal [@@alert "-internal"]

  type t = string * Out_channel.t (* Filename and corresponding channel *)

  type cmd = Flush of Var.t | Close of Var.t | Write of Var.t * string [@@deriving show { with_path = false }]
(*
  let show_cmd =
    let open Printf in function
    | Flush -> "Flush"
    | Write s -> sprintf "Write %s" s
    | Close -> "Close"
*)
  let gen_cmd gen_var =
    let open QCheck.Gen in
    frequency
      [3, map  (fun i -> None,Flush i) gen_var;
       1, map  (fun i -> None,Close i) gen_var;
       6, map2 (fun i s -> None,Write (i,s)) gen_var string;
      ]

  let shrink_cmd _env _ = QCheck.Iter.empty

  let cmd_uses_var v = function
    | Flush i
    | Close i
    | Write (i,_) -> i=v

  type res = (unit, exn) result

  let show_res =
    let open Printf in function
    | Ok () -> sprintf "()"
    | Error e -> sprintf "exception %s" (Printexc.to_string e)

  let equal_res = (=)

  let init () =
    let filename = Filename.temp_file "fuzz_stdlib" "" in
    filename, Out_channel.open_text filename

  let cleanup (filename, chan) =
    Out_channel.close chan;
    try Sys.remove filename with Sys_error _ -> ()

  let run cmd chan =
    match cmd with
    | None, Flush i ->
        begin try Out_channel.flush (snd chan.(i)); Ok ()
        with e -> Error e
        end
    | None, Write (i,s) ->
        begin try Out_channel.output_string (snd chan.(i)) s; Ok ()
        with e -> Error e
        end
    | None, Close i ->
        begin try Out_channel.close (snd chan.(i)); Ok ()
        with e -> Error e
        end
    | _, _ -> failwith (Printf.sprintf "unexpected command: %s" (show_cmd (snd cmd)))
end

module Out_channel_lin = Lin_domain.Make_internal (Out_channel_ops) [@@alert "-internal"]
module In_channel_lin = Lin_domain.Make_internal (In_channel_ops) [@@alert "-internal"]

let () =
  QCheck_base_runner.run_tests_main
    [ Out_channel_lin.lin_test ~count:1000 ~name:"Lin Out_channel test with domains";
      In_channel_lin.lin_test ~count:1000 ~name:"Lin In_channel test with domains";
    ]

let () =
  Sys.remove temp_readonly
