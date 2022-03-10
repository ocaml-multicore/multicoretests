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

  type t = In_channel.t (* Filename and corresponding channel *)

  type cmd = Close | Read of int | BlindRead of int

  let show_cmd =
    let open Printf in function
    | Close -> "Close"
    | Read l -> sprintf "Read %d" l
    | BlindRead l -> sprintf "BlindRead %d" l

  let gen_cmd =
    let open QCheck.Gen in
    frequency
      [1, return Close;
       6, map (fun l -> Read l) small_nat;
       6, map (fun l -> BlindRead l) small_nat;
      ]

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

  let init () =
    In_channel.open_bin temp_readonly

  let cleanup chan =
    In_channel.close chan

  let run cmd chan =
    match cmd with
    | Read l ->
        begin try ReadRes (Ok (In_channel.really_input_string chan l))
        with e -> ReadRes (Error e)
        end
    | BlindRead l ->
        begin try ignore (In_channel.really_input_string chan l); UnitRes (Ok ())
        with e -> UnitRes (Error e)
        end
    | Close ->
        begin try In_channel.close chan; UnitRes (Ok ())
        with e -> UnitRes (Error e)
        end
end

module Out_channel_ops = struct

  type t = string * Out_channel.t (* Filename and corresponding channel *)

  type cmd = Flush | Close | Write of string

  let show_cmd =
    let open Printf in function
    | Flush -> "Flush"
    | Write s -> sprintf "Write %s" s
    | Close -> "Close"

  let gen_cmd =
    let open QCheck.Gen in
    frequency
      [3, return Flush;
       1, return Close;
       6, map (fun s -> Write s) string;
      ]

  type res = (unit, exn) result

  let show_res =
    let open Printf in function
    | Ok () -> sprintf "()"
    | Error e -> sprintf "exception %s" (Printexc.to_string e)

  let init () =
    let filename = Filename.temp_file "fuzz_stdlib" "" in
    filename, Out_channel.open_text filename

  let cleanup (filename, chan) =
    Out_channel.close chan;
    try Sys.remove filename with Sys_error _ -> ()

  let run cmd (_,chan) =
    match cmd with
    | Flush ->
        begin try Out_channel.flush chan; Ok ()
        with e -> Error e
        end
    | Write s ->
        begin try Out_channel.output_string chan s; Ok ()
        with e -> Error e
        end
    | Close ->
        begin try Out_channel.close chan; Ok ()
        with e -> Error e
        end
end

module Out_channel_lin = Lin.Make (Out_channel_ops)
module In_channel_lin = Lin.Make (In_channel_ops)

let () =
  QCheck_runner.run_tests_main
    [ Out_channel_lin.lin_test ~count:1000 ~name:"Out_channel" `Domain;
      In_channel_lin.lin_test ~count:1000 ~name:"In_channel" `Domain;
    ]

let () =
  Sys.remove temp_readonly
