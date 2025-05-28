module SConf =
struct
  type path = string list

  type cmd =
    | Rename of path * path
    | Mkdir of path * string
    | Rmdir of path * string
    | Mkfile of path * string

  let pp_cmd par fmt x =
    let open Util.Pp in
    let pp_path = pp_list pp_string in
    match x with
    | Rename (x, y) -> cst2 pp_path pp_path "Rename" par fmt x y
    | Mkdir (x, y) -> cst2 pp_path pp_string "Mkdir" par fmt x y
    | Rmdir (x, y) -> cst2 pp_path pp_string "Rmdir" par fmt x y
    | Mkfile (x, y) -> cst2 pp_path pp_string "Mkfile" par fmt x y

  let show_cmd = Util.Pp.to_show pp_cmd

  let (/) = Filename.concat

  let sandbox_root = "_sandbox"

  let init_sut () =
    try Sys.mkdir sandbox_root 0o700
    with Sys_error msg when msg = sandbox_root ^ ": File exists" -> ()

  let cleanup _ =
    match Sys.os_type with
    | "Cygwin"
    | "Unix"  -> ignore (Sys.command ("rm -r " ^ Filename.quote sandbox_root))
    | "Win32" -> ignore (Sys.command ("rd /s /q " ^ Filename.quote sandbox_root))
    | v -> failwith ("Sys tests not working with " ^ v)

  let p path =  (List.fold_left (/) sandbox_root path)

  let mkfile filepath =
    let flags = [Open_wronly; Open_creat; Open_excl] in
    Out_channel.with_open_gen flags 0o666 filepath (fun _ -> ())

  let protect (f : 'a -> 'b) (a : 'a) : ('b, exn) result =
    try Result.Ok (f a)
    with e -> Result.Error e

  let run c _file_name =
    match c with
    | Rename (old_path, new_path) -> protect (Sys.rename (p old_path)) (p new_path)
    | Mkdir (path, new_dir_name) -> protect (Sys.mkdir ((p path) / new_dir_name)) 0o755
    | Rmdir (path, delete_dir_name) -> protect (Sys.rmdir) ((p path) / delete_dir_name)
    | Mkfile (path, new_file_name) -> protect mkfile (p path / new_file_name)
end

let rep_count = 50 (* No. of repetitions of the non-deterministic property *)

let triple =
  let open SConf in
  ([Mkdir ([], "hhh");                       (* Sys.mkdir "hhh" 0o755;; - : unit = () *)
    Mkfile (["hhh"], "iii");                 (* mkfile ("hhh" / "iii");; - : unit = () *)
    Mkdir (["hhh"], "hhh")],                 (* Sys.mkdir ("hhh" / "hhh") 0o755;; - : unit = () *)

   [Mkdir (["hhh"; "iii"], "eee");           (* Sys.mkdir ("hhh" / "iii" / "eee") 0o755;; Exception: Sys_error "hhh/iii/eee: Not a directory". *)
    Rename (["hhh"; "hhh"], []);             (* Sys.rename ("hhh" / "hhh") "";; Exception: Sys_error "No such file or directory". *)
    Rename (["bbb"], [])],                   (* Sys.rename ("bbb") "";; Exception: Sys_error "No such file or directory". *)

   [Rename (["hhh"; "iii"], ["iii"; "ccc"]); (* Sys.rename ("hhh" / "iii") ("iii" / "ccc");; Exception: Sys_error "No such file or directory". *)
    Rmdir ([], "hhh");                       (* Sys.rmdir "hhh";; Exception: Sys_error "hhh: Directory not empty". *)
    Mkdir (["hhh"], "iii");                  (* Sys.mkdir ("hhh" / "iii") 0o755;; Exception: Sys_error "hhh/iii: File exists". *)
    Rmdir ([], "hhh")])                      (* Sys.rmdir "hhh";; Exception: Sys_error "hhh: Directory not empty". *)

(* operate over arrays to avoid needless allocation underway *)
let interp_sut_res sut cs =
  let cs_arr = Array.of_list cs in
  let res_arr = Array.map (fun c -> Domain.cpu_relax(); SConf.run c sut) cs_arr in
  List.combine cs (Array.to_list res_arr)

let run_par seq_pref cmds1 cmds2 =
  let sut = SConf.init_sut () in
  let pref_obs = interp_sut_res sut seq_pref in
  let barrier = Atomic.make 2 in
  let main cmds () =
    Atomic.decr barrier;
    while Atomic.get barrier <> 0 do Domain.cpu_relax() done;
    try Ok (interp_sut_res sut cmds) with exn -> Error exn
  in
  let dom1 = Domain.spawn (main cmds1) in
  let dom2 = Domain.spawn (main cmds2) in
  let obs1 = Domain.join dom1 in
  let obs2 = Domain.join dom2 in
  let ()   = SConf.cleanup sut in
  let obs1 = match obs1 with Ok v -> v | Error exn -> raise exn in
  let obs2 = match obs2 with Ok v -> v | Error exn -> raise exn in
  pref_obs, obs1, obs2

let stress_prop_par (seq_pref,cmds1,cmds2) =
  let _ = run_par seq_pref cmds1 cmds2 in
  true

let rec repeat n prop input =
  if n<0 then failwith "repeat: negative repetition count";
  if n=0
  then true
  else prop input && repeat (n-1) prop input

let stress_test_par () =
  repeat rep_count stress_prop_par triple |> ignore (* 25 times each, then 25 * 10 times when shrinking *)

let _ =
  Printf.printf "%s\n\n%!"
    @@ Util.print_triple_vertical ~fig_indent:5 ~res_width:35 SConf.show_cmd triple;
  for i=1 to 1000 do
    Printf.printf "Iteration %i\n%!" i;
    stress_test_par ()
  done
