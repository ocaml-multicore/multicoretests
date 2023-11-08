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

  let shrink_cmd _ = QCheck.Iter.empty

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

  type t = Out_channel.t
  let path = ref ""

  type cmd =
    | Seek of int64
    | Close
    | Flush
    | Output_string of string
    | Set_binary_mode of bool
    | Set_buffered of bool
    | Is_buffered

  let show_cmd =
    let open Printf in function
    | Seek i -> sprintf "Seek %Li" i
    | Close -> "Close"
    | Flush -> "Flush"
    | Output_string s -> sprintf "Output_string %s" s
    | Set_binary_mode b -> sprintf "Set_binary_mode %s" QCheck.Print.(bool b)
    | Set_buffered b -> sprintf "Set_buffered %s" QCheck.Print.(bool b)
    | Is_buffered -> "Is_buffered"

  let gen_cmd =
    let open QCheck.Gen in
    frequency
      [10, map (fun i -> Seek (Int64.of_int i)) small_nat;
       10, return Close;
       10, return Flush;
       10, map (fun s -> Output_string s) string_small;
       10, map (fun b -> Set_binary_mode b) bool;
       10, map (fun b -> Set_buffered b) bool;
       10, return Is_buffered;
      ]

  let shrink_cmd _ = QCheck.Iter.empty

  type inner_res = Unit | Bool of bool
  type res = (inner_res, exn) result

  let show_res =
    let open Printf in function
      | Ok r -> (match r with
          | Unit -> sprintf "()"
          | Bool b -> QCheck.Print.(bool b)
        )
      | Error e -> sprintf "exception %s" (Printexc.to_string e)

  let equal_res = (=)

  let init () =
    let p,ch = Filename.open_temp_file "lin-internal-" "" in
    path := p;
    ch

  let cleanup chan =
    Out_channel.close chan;
    Sys.remove !path

  let run cmd chan =
    match cmd with
    | Seek i ->
      (try Out_channel.seek chan i; Ok Unit with e -> Error e)
    | Close ->
      (try Out_channel.close chan; Ok Unit with e -> Error e)
    | Flush ->
      (try Out_channel.flush chan; Ok Unit with e -> Error e)
    | Output_string s ->
      (try Out_channel.output_string chan s; Ok Unit with e -> Error e)
    | Set_binary_mode b ->
      (try Out_channel.set_binary_mode chan b; Ok Unit with e -> Error e)
    | Set_buffered b ->
      (try Out_channel.set_buffered chan b; Ok Unit with e -> Error e)
    | Is_buffered ->
      (try Ok (Bool (Out_channel.is_buffered chan)) with e -> Error e)

end

module In_channel_lin = Lin_domain.Make_internal (In_channel_ops) [@@alert "-internal"]
module Out_channel_lin = Lin_domain.Make_internal (Out_channel_ops) [@@alert "-internal"]

let () =
  QCheck_base_runner.run_tests_main
    [ In_channel_lin.lin_test ~count:1000 ~name:"Lin.Internal In_channel test with domains";
      Out_channel_lin.lin_test ~count:1000 ~name:"Lin.Internal Out_channel test with domains";
    ]

let () =
  Sys.remove temp_readonly
