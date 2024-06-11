open QCheck
open STM

module SConf =
struct
  type path = string list

  type cmd =
    | File_exists of path
    | Mkdir of path * string
    | Rmdir of path * string
    | Readdir of path
    | Mkfile of path * string

  let pp_cmd par fmt x =
    let open Util.Pp in
    let pp_path = pp_list pp_string in
    match x with
    | File_exists x -> cst1 pp_path "File_exists" par fmt x
    | Mkdir (x, y) -> cst2 pp_path pp_string "Mkdir" par fmt x y
    | Rmdir (x, y) -> cst2 pp_path pp_string "Rmdir" par fmt x y
    | Readdir x -> cst1 pp_path "Readdir" par fmt x
    | Mkfile (x, y) -> cst2 pp_path pp_string "Mkfile" par fmt x y

  let show_cmd = Util.Pp.to_show pp_cmd

  module Map_names = Map.Make (String)

  type filesys =
    | Directory of {fs_map: filesys Map_names.t}
    | File

  type state = filesys

  type sut   = unit

  let (/) = Filename.concat

  let update_map_name map_name k v = Map_names.update k (fun _ -> Some v) map_name

  (* var gen_existing_path : filesys -> path Gen.t *)
  let rec gen_existing_path fs =
    match fs with
    | File -> Gen.return []
    | Directory d ->
      (match Map_names.bindings d.fs_map with
      | [] -> Gen.return []
      | bindings -> Gen.(oneofl bindings >>= fun (n, sub_fs) ->
        Gen.oneof [
          Gen.return [n];
          Gen.map (fun l -> n::l) (gen_existing_path sub_fs)]
        )
      )

  (* var gen_existing_pair : filesys -> (path * string) option Gen.t *)
  let rec gen_existing_pair fs = match fs with
    | File -> Gen.return None (*failwith "no sandbox directory"*)
    | Directory d ->
      (match Map_names.bindings d.fs_map with
      | [] -> Gen.return None
      | bindings ->
        Gen.(oneofl bindings >>= fun (n, sub_fs) ->
             oneof [
               return (Some ([],n));
               map (function None -> Some ([],n)
                           | Some (path,name) -> Some (n::path,name)) (gen_existing_pair sub_fs)]
            )
      )

  let name_gen = Gen.oneofl ["aaa" ; "bbb" ; "ccc" ; "ddd" ; "eee"]
  let path_gen s = Gen.(oneof [gen_existing_path s; list_size (int_bound 5) name_gen]) (* can be empty *)
  let pair_gen s =
    let fresh_pair_gen = Gen.(pair (list_size (int_bound 5) name_gen)) name_gen in
    Gen.(oneof [
        fresh_pair_gen;
        (gen_existing_pair s >>= function None -> fresh_pair_gen
                                        | Some (p,_) -> map (fun n -> (p,n)) name_gen);
        (gen_existing_pair s >>= function None -> fresh_pair_gen
                                        | Some (p,n) -> return (p,n));
      ])

  let arb_cmd s =
    QCheck.make ~print:show_cmd
      Gen.(oneof [
          map (fun path -> File_exists path) (path_gen s);
          map (fun (path,new_dir_name) -> Mkdir (path, new_dir_name)) (pair_gen s);
          map (fun (path,delete_dir_name) -> Rmdir (path, delete_dir_name)) (pair_gen s);
          map (fun path -> Readdir path) (path_gen s);
          map (fun (path,new_file_name) -> Mkfile (path, new_file_name)) (pair_gen s);
        ])

  let sandbox_root = "_sandbox"

  let init_state  = Directory {fs_map = Map_names.empty}

  let rec find_opt_model fs path =
    match fs with
    | File ->
      if path = []
      then Some fs
      else None
    | Directory d ->
      (match path with
      | []       -> Some (Directory d)
      | hd :: tl ->
        (match Map_names.find_opt hd d.fs_map with
        | None    -> None
        | Some fs -> find_opt_model fs tl))

  let mem_model fs path = find_opt_model fs path <> None

  let rec mkdir_model fs path new_dir_name =
    match fs with
    | File -> fs
    | Directory d ->
      (match path with
      | [] ->
        let new_dir = Directory {fs_map = Map_names.empty} in
        Directory {fs_map = Map_names.add new_dir_name new_dir d.fs_map}
      | next_in_path :: tl_path ->
        (match Map_names.find_opt next_in_path d.fs_map with
        | None -> fs
        | Some sub_fs ->
          let nfs = mkdir_model sub_fs tl_path new_dir_name in
          if nfs = sub_fs
          then fs
          else
            let new_map = Map_names.remove next_in_path d.fs_map in
            let new_map = Map_names.add next_in_path nfs new_map in
            Directory {fs_map = new_map}))

  let readdir_model fs path =
    match find_opt_model fs path with
    | None    -> None
    | Some fs ->
      (match fs with
      | File -> None
      | Directory d -> Some (Map_names.fold (fun k _ l -> k::l) d.fs_map []))

  let rec rmdir_model fs path delete_dir_name =
    match fs with
    | File        -> fs
    | Directory d ->
      (match path with
      | [] ->
        (match Map_names.find_opt delete_dir_name d.fs_map with
        | Some (Directory target) when Map_names.is_empty target.fs_map ->
          Directory {fs_map = Map_names.remove delete_dir_name d.fs_map}
        | None | Some File | Some (Directory _) -> fs)
      | next_in_path :: tl_path ->
        (match Map_names.find_opt next_in_path d.fs_map with
        | None        -> fs
        | Some sub_fs ->
          let nfs = rmdir_model sub_fs tl_path delete_dir_name in
          if nfs = sub_fs
          then fs
          else Directory {fs_map = (update_map_name d.fs_map next_in_path nfs)}))

  let rec mkfile_model fs path new_file_name =
    match fs with
    | File        -> fs
    | Directory d ->
      (match path with
      | [] ->
        let new_file = File in
        Directory {fs_map = Map_names.add new_file_name new_file d.fs_map}
      | next_in_path :: tl_path ->
        (match Map_names.find_opt next_in_path d.fs_map with
        | None        -> fs
        | Some sub_fs ->
          let nfs = mkfile_model sub_fs tl_path new_file_name in
          if nfs = sub_fs
          then fs
          else Directory {fs_map = update_map_name d.fs_map next_in_path nfs}))

  let next_state c fs =
    match c with
    | File_exists _path -> fs
    | Mkdir (path, new_dir_name) ->
      if mem_model fs (path @ [new_dir_name])
      then fs
      else mkdir_model fs path new_dir_name
    | Rmdir (path,delete_dir_name) ->
      if mem_model fs (path @ [delete_dir_name])
      then rmdir_model fs path delete_dir_name
      else fs
    | Readdir _path -> fs
    | Mkfile (path, new_file_name) ->
      if mem_model fs (path @ [new_file_name])
      then fs
      else mkfile_model fs path new_file_name

  let init_sut () =
    try Sys.mkdir sandbox_root 0o700
    with Sys_error msg when msg = sandbox_root ^ ": File exists" -> ()

  let cleanup _ =
    match Sys.os_type with
    | "Cygwin"
    | "Unix"  -> ignore (Sys.command ("rm -r " ^ Filename.quote sandbox_root))
    | "Win32" -> ignore (Sys.command ("rd /s /q " ^ Filename.quote sandbox_root))
    | v -> failwith ("Sys tests not working with " ^ v)

  let precond _c _s = true

  let p path =  (List.fold_left (/) sandbox_root path)

  let mkfile filepath =
    let flags = [Open_wronly; Open_creat; Open_excl] in
    Out_channel.with_open_gen flags 0o666 filepath (fun _ -> ())

  let run c _file_name =
    match c with
    | File_exists path -> Res (bool, Sys.file_exists (p path))
    | Mkdir (path, new_dir_name) ->
      Res (result unit exn, protect (Sys.mkdir ((p path) / new_dir_name)) 0o755)
    | Rmdir (path, delete_dir_name) ->
      Res (result unit exn, protect (Sys.rmdir) ((p path) / delete_dir_name))
    | Readdir path ->
      Res (result (array string) exn, protect (Sys.readdir) (p path))
    | Mkfile (path, new_file_name) ->
      Res (result unit exn, protect mkfile (p path / new_file_name))

  let fs_is_a_dir fs = match fs with | Directory _ -> true | File -> false

  let path_is_a_dir fs path =
    match find_opt_model fs path with
    | None -> false
    | Some target_fs -> fs_is_a_dir target_fs

  let postcond c (fs: filesys) res =
    match c, res with
    | File_exists path, Res ((Bool,_),b) -> b = mem_model fs path
    | Mkdir (path, new_dir_name), Res ((Result (Unit,Exn),_), res) ->
      let complete_path = (path @ [new_dir_name]) in
      (match res with
      | Error err ->
        (match err with
        | Sys_error s ->
          (s = (p complete_path) ^ ": Permission denied") ||
          (s = (p complete_path) ^ ": File exists" && mem_model fs complete_path) ||
          ((s = (p complete_path) ^ ": No such file or directory"
            || s = (p complete_path) ^ ": Invalid argument") && not (mem_model fs path)) ||
          if Sys.win32 && not (path_is_a_dir fs complete_path)
          then s = (p complete_path) ^ ": No such file or directory"
          else s = (p complete_path) ^ ": Not a directory"
          | _ -> false)
        | Ok () -> mem_model fs path && path_is_a_dir fs path && not (mem_model fs complete_path))
    | Rmdir (path, delete_dir_name), Res ((Result (Unit,Exn),_), res) ->
      let complete_path = (path @ [delete_dir_name]) in
      (match res with
      | Error err ->
        (match err with
          | Sys_error s ->
            (s = (p complete_path) ^ ": Permission denied") ||
            (s = (p complete_path) ^ ": Directory not empty" && not (readdir_model fs complete_path = Some [])) ||
            (s = (p complete_path) ^ ": No such file or directory" && not (mem_model fs complete_path)) ||
            if Sys.win32 && not (path_is_a_dir fs complete_path) (* if not a directory *)
            then s = (p complete_path) ^ ": Invalid argument"
            else s = (p complete_path) ^ ": Not a directory"
          | _ -> false)
      | Ok () ->
          mem_model fs complete_path && path_is_a_dir fs complete_path && readdir_model fs complete_path = Some [])
    | Readdir path, Res ((Result (Array String,Exn),_), res) ->
      (match res with
      | Error err ->
        (match err with
          | Sys_error s ->
            (s = (p path) ^ ": Permission denied") ||
            (s = (p path) ^ ": No such file or directory" && not (mem_model fs path)) ||
            if Sys.win32 && not (path_is_a_dir fs path) (* if not a directory *)
            then s = (p path) ^ ": Invalid argument"
            else s = (p path) ^ ": Not a directory"
          | _ -> false)
      | Ok array_of_subdir ->
        (* Temporary work around for mingW, see https://github.com/ocaml/ocaml/issues/11829 *)
        if Sys.win32 && not (mem_model fs path)
        then array_of_subdir = [||]
        else
          (mem_model fs path && path_is_a_dir fs path &&
          (match readdir_model fs path with
          | None   -> false
          | Some l ->
            List.sort String.compare l
            = List.sort String.compare (Array.to_list array_of_subdir))))
    | Mkfile (path, new_file_name), Res ((Result (Unit,Exn),_),res) -> (
      let complete_path = path @ [ new_file_name ] in
      let concatenated_path = p complete_path in
      let match_msg err msg = err = concatenated_path ^ ": " ^ msg in
      let match_msgs err = List.exists (match_msg err) in
      let msgs_already_exists = ["File exists"; "Permission denied"]
          (* Permission denied: seen (sometimes?) on Windows *)
      and msgs_non_existent_dir = ["No such file or directory";
                                   "Invalid argument";
                                   "Permission denied"]
          (* Invalid argument: seen on macOS
             Permission denied: seen on Windows *)
      and msg_path_not_dir =
        match Sys.os_type with
        | "Cygwin"
        | "Unix"  -> "Not a directory"
        | "Win32" -> "No such file or directory"
        | v -> failwith ("Sys tests not working with " ^ v)
      in
      match res with
      | Error err -> (
        match err with
        | Sys_error s ->
             (mem_model fs complete_path  && match_msgs s msgs_already_exists)
          || (not (mem_model fs path)     && match_msgs s msgs_non_existent_dir)
          || (not (path_is_a_dir fs path) && match_msg  s msg_path_not_dir)
        | _ -> false)
      | Ok () -> path_is_a_dir fs path && not (mem_model fs complete_path))
    | _,_ -> false
end

let run_cmd cmd =
  let ic = Unix.open_process_in cmd in
  let os = In_channel.input_line ic in
  ignore (Unix.close_process_in ic);
  os

let uname_os () = run_cmd "uname -s"

module Sys_seq = STM_sequential.Make(SConf)
module Sys_dom = STM_domain.Make(SConf)

;;
QCheck_base_runner.run_tests_main [
    Sys_seq.agree_test              ~count:1000 ~name:"STM Sys test sequential";
    if Sys.unix && uname_os () = Some "Linux"
    then Sys_dom.agree_test_par     ~count:200  ~name:"STM Sys test parallel"
    else Sys_dom.neg_agree_test_par ~count:2500 ~name:"STM Sys test parallel";
    Sys_dom.stress_test_par         ~count:1000 ~name:"STM Sys stress test parallel";
  ]
