open QCheck
open STM

module Model =
struct
  module Map_names = Map.Make (String)

  type filesys =
    | Directory of {fs_map: filesys Map_names.t}
    | File

  let empty_dir = Directory {fs_map = Map_names.empty}

  let rec find_opt fs path =
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
          | Some fs -> find_opt fs tl))

  let mem fs path = find_opt fs path <> None

  (* generic removal function *)
  let rec remove fs path file_name =
    match fs with
    | File -> fs
    | Directory d ->
      (match path with
       | [] ->
         (match Map_names.find_opt file_name d.fs_map with
          | None
          | Some _ -> Directory { fs_map = Map_names.remove file_name d.fs_map })
       | dir::dirs ->
         Directory
           { fs_map = Map_names.update dir (function
                 | None -> None
                 | Some File -> Some File
                 | Some (Directory _ as d') -> Some (remove d' dirs file_name)) d.fs_map
           })

  let readdir fs path =
    match find_opt fs path with
    | None    -> None
    | Some fs ->
      (match fs with
       | File -> None
       | Directory d -> Some (Map_names.fold (fun k _ l -> k::l) d.fs_map []))

  let update_map_name map_name k v = Map_names.update k (fun _ -> Some v) map_name

  (* generic insertion function *)
  let rec insert fs path new_file_name sub_tree =
    match fs with
    | File        -> fs
    | Directory d ->
      (match path with
       | [] ->
         Directory {fs_map = Map_names.add new_file_name sub_tree d.fs_map}
       | next_in_path :: tl_path ->
         (match Map_names.find_opt next_in_path d.fs_map with
          | None        -> fs
          | Some sub_fs ->
            let nfs = insert sub_fs tl_path new_file_name sub_tree in
            if nfs = sub_fs
            then fs
            else Directory {fs_map = update_map_name d.fs_map next_in_path nfs}))

  let separate_path path =
    match List.rev path with
    | [] -> None
    | name::rev_path -> Some (List.rev rev_path, name)

  let rename fs old_path new_path =
    match separate_path old_path, separate_path new_path with
    | None, _
    | _, None -> fs
    | Some (old_path_pref, old_name), Some (new_path_pref, new_name) ->
      (match find_opt fs new_path_pref with
       | None
       | Some File -> fs
       | Some (Directory _) ->
         (match find_opt fs old_path with
          | None -> fs
          | Some sub_fs ->
            let fs' = remove fs old_path_pref old_name in
            insert fs' new_path_pref new_name sub_fs))

  let path_is_a_file fs path =
    match find_opt fs path with
    | None
    | Some (Directory _) -> false
    | Some File -> true

  let path_is_a_dir fs path =
    match find_opt fs path with
    | None
    | Some File -> false
    | Some (Directory _) -> true
end

module SConf =
struct
  type path = string list

  type cmd =
    | File_exists of path
    | Is_directory of path
    | Remove of path * string
    | Rename of path * path
    | Mkdir of path * string
    | Rmdir of path * string
    | Readdir of path
    | Mkfile of path * string

  let pp_cmd par fmt x =
    let open Util.Pp in
    let pp_path = pp_list pp_string in
    match x with
    | File_exists x -> cst1 pp_path "File_exists" par fmt x
    | Is_directory x -> cst1 pp_path "Is_directory" par fmt x
    | Remove (x, y) -> cst2 pp_path pp_string "Remove" par fmt x y
    | Rename (x, y) -> cst2 pp_path pp_path "Rename" par fmt x y
    | Mkdir (x, y) -> cst2 pp_path pp_string "Mkdir" par fmt x y
    | Rmdir (x, y) -> cst2 pp_path pp_string "Rmdir" par fmt x y
    | Readdir x -> cst1 pp_path "Readdir" par fmt x
    | Mkfile (x, y) -> cst2 pp_path pp_string "Mkfile" par fmt x y

  let show_cmd = Util.Pp.to_show pp_cmd

  type state = Model.filesys

  type sut   = unit

  let (/) = Filename.concat

  (* var existing_contents : filesys -> path list * path list *)
  let rec existing_contents fs : path list * path list =
    match fs with
    | Model.File -> [[]],[]
    | Model.Directory d ->
      let bindings = Model.Map_names.bindings d.fs_map in
      let files, dirs = List.partition (fun p -> snd p = Model.File) bindings in
      let sub_res =
        List.map (fun (n,sub_fs) ->
            let sub_files, sub_dirs = existing_contents sub_fs in
            List.map (fun l -> n::l) sub_files,
            List.map (fun l -> n::l) sub_dirs) dirs in
      let files = List.map (fun (n,_) -> [n]) files in
      List.concat (files :: List.map fst sub_res),
      []::List.concat (List.map snd sub_res)

  let name_gen = Gen.oneofl ["aaa" ; "bbb" ; "ccc" ; "ddd" ; "eee"; "fff"; "ggg"; "hhh"; "iii"]

  let arb_cmd s =
    let files, dirs = existing_contents s in
    let gen_file = Gen.oneofl files in
    let gen_file_sep = Gen.oneofl (List.filter_map Model.separate_path files) in
    let gen_dir = Gen.oneofl dirs in
    let gen_dir_sep = Gen.oneofl (List.filter_map Model.separate_path dirs) in
    let gen_arb_path = Gen.(list_size (int_bound 5) name_gen) in
    let gen_arb_path_sep = Gen.(pair (list_size (int_bound 4) name_gen) name_gen) in
    QCheck.make ~print:show_cmd
      Gen.(
        if files = []
        then
          oneof [
            map2 (fun path new_file_name -> Mkfile (path, new_file_name)) gen_dir name_gen;
            map2 (fun path new_dir_name -> Mkdir (path, new_dir_name)) gen_dir name_gen;
          ]
        else
          frequency [
          (*1,map (fun path -> File_exists path) (frequency [8,gen_file; 1,gen_dir; 1,gen_arb_path]);*)
          (*1,map (fun path -> Is_directory path) (frequency [1,gen_file; 8,gen_dir; 1,gen_arb_path]);*)
          (*1,map (fun (path,file_name) -> Remove (path, file_name)) (if List.length dirs > 1
                                                                      then frequency [8,gen_file_sep; 1,gen_dir_sep; 1,gen_arb_path_sep]
                                                                      else frequency [1,gen_file_sep; 1,gen_arb_path_sep]);*)
            3,map (fun (old_path,new_path) -> Rename (old_path, new_path)) (frequency [5,(pair gen_file gen_arb_path);
                                                                                       5,(pair gen_dir gen_arb_path);
                                                                                       1,(pair gen_arb_path gen_arb_path);
                                                                                      ]);
            3,map2 (fun path new_dir_name -> Mkdir (path, new_dir_name)) (frequency [1,gen_file; 8,gen_dir; 1,gen_arb_path]) name_gen;
            1,map (fun (path,dir_name) -> Rmdir (path, dir_name)) (if List.length dirs > 1
                                                                   then frequency [1,gen_file_sep; 8,gen_dir_sep; 1,gen_arb_path_sep]
                                                                   else gen_arb_path_sep);
            1,map (fun path -> Readdir path) (frequency [1,gen_file; 8,gen_dir; 1,gen_arb_path]);
            3,map2 (fun path new_file_name -> Mkfile (path, new_file_name)) gen_dir name_gen;
        ])

  let sandbox_root = "_sandbox"

  let init_state  = Model.empty_dir

  let path_is_an_empty_dir fs path =
    Model.readdir fs path = Some []

  let rec path_prefixes path = match path with
    | [] -> []
    | [_] -> []
    | n::ns -> [n]::(List.map (fun p -> n::p) (path_prefixes ns))

  let rec is_true_prefix path1 path2 = match path1, path2 with
    | [], [] -> false
    | [], _::_ -> true
    | _::_, [] -> false
    | n1::p1, n2::p2 -> n1=n2 && is_true_prefix p1 p2

  (* Note: This model-based test has previously found a number of issues under MinGW/MSVC:
     - non-existing readdir on MinGW https://github.com/ocaml/ocaml/issues/11829 (5.1)
     - rename dir-to-empty-target-dir https://github.com/ocaml/ocaml/issues/12073 (5.1)
     - rename dir-to-file https://github.com/ocaml/ocaml/issues/12073 (5.1)
     - rename empty-dir to itself (regression) https://github.com/ocaml/ocaml/issues/12317 (5.1)
     - rename parent-to-empty-child-dir https://github.com/ocaml/ocaml/pull/13166 (5.3)
     These issues have since been fixed and the test workarounds have therefore been removed again.
     As a result this test may fail on MinGW/MSVC with OCaml 5.0-5.2.
  *)
  let next_state c fs =
    match c with
    | File_exists _path -> fs
    | Mkdir (path, new_dir_name) ->
      if Model.mem fs (path @ [new_dir_name])
      then fs
      else Model.insert fs path new_dir_name Model.empty_dir
    | Remove (path, file_name) ->
      if Model.find_opt fs (path @ [file_name]) = Some File
      then Model.remove fs path file_name
      else fs
    | Rename (old_path, new_path) ->
      if is_true_prefix old_path new_path
      then fs
      else
        (match Model.find_opt fs old_path with
         | None -> fs
         | Some File ->
           if (not (Model.mem fs new_path) || Model.path_is_a_file fs new_path) then Model.rename fs old_path new_path else fs
         | Some (Directory _) ->
           if (not (Model.mem fs new_path) || path_is_an_empty_dir fs new_path) then Model.rename fs old_path new_path else fs)
    | Is_directory _path -> fs
    | Rmdir (path,delete_dir_name) ->
      let complete_path = path @ [delete_dir_name] in
      if Model.mem fs complete_path && path_is_an_empty_dir fs complete_path
      then Model.remove fs path delete_dir_name
      else fs
    | Readdir _path -> fs
    | Mkfile (path, new_file_name) ->
      if Model.mem fs (path @ [new_file_name])
      then fs
      else Model.insert fs path new_file_name File

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
    | Is_directory path -> Res (result bool exn, protect Sys.is_directory (p path))
    | Remove (path, file_name) -> Res (result unit exn, protect Sys.remove ((p path) / file_name))
    | Rename (old_path, new_path) -> Res (result unit exn, protect (Sys.rename (p old_path)) (p new_path))
    | Mkdir (path, new_dir_name) ->
      Res (result unit exn, protect (Sys.mkdir ((p path) / new_dir_name)) 0o755)
    | Rmdir (path, delete_dir_name) ->
      Res (result unit exn, protect (Sys.rmdir) ((p path) / delete_dir_name))
    | Readdir path ->
      Res (result (array string) exn, protect (Sys.readdir) (p path))
    | Mkfile (path, new_file_name) ->
      Res (result unit exn, protect mkfile (p path / new_file_name))

  let postcond c (fs: Model.filesys) res =
    match c, res with
    | File_exists path, Res ((Bool,_),b) ->
      b = Model.mem fs path
    | Is_directory path, Res ((Result (Bool,Exn),_),res) ->
      (match res with
       | Ok b ->
         (match Model.find_opt fs path with
          | Some (Directory _) -> b = true
          | Some File -> b = false
          | None -> false)
       | Error (Sys_error _) -> not (Model.mem fs path)
       | _ -> false)
    | Remove (path, file_name), Res ((Result (Unit,Exn),_), res) ->
      let full_path = path @ [file_name] in
      (match res with
       | Ok () -> Model.mem fs full_path && Model.path_is_a_dir fs path && not (Model.path_is_a_dir fs full_path)
       | Error (Sys_error _) ->
         (not (Model.mem fs full_path)) || Model.path_is_a_dir fs full_path || not (Model.path_is_a_dir fs path)
       | Error _ -> false
      )
    | Rename (old_path, new_path), Res ((Result (Unit,Exn),_), res) ->
      (match res with
       | Ok () -> Model.mem fs old_path (* permits dir-to-file MingW success https://github.com/ocaml/ocaml/issues/12073 *)
       | Error (Sys_error _) ->
         (* general conditions *)
         (not (Model.mem fs old_path)) ||
         is_true_prefix old_path new_path || (* parent-to-child *)
         is_true_prefix new_path old_path || (* child-to-parent *)
         (Model.path_is_a_file fs old_path && Model.path_is_a_dir fs new_path) || (* file-to-dir *)
         (Model.path_is_a_dir fs old_path && Model.path_is_a_file fs new_path) || (* dir-to-file *)
         (Model.path_is_a_dir fs new_path && not (path_is_an_empty_dir fs new_path)) || (* to-non-empty-dir *)
         List.exists (fun pref -> not (Model.path_is_a_dir fs pref)) (path_prefixes new_path) (* malformed-target-path *)
       | Error _ -> false)
    | Mkdir (path, new_dir_name), Res ((Result (Unit,Exn),_), res) ->
      let full_path = path @ [new_dir_name] in
      (match res with
       | Ok () -> Model.mem fs path && Model.path_is_a_dir fs path && not (Model.mem fs full_path)
       | Error (Sys_error _) ->
         Model.mem fs full_path || (not (Model.mem fs path)) || not (Model.path_is_a_dir fs full_path)
       | Error _ -> false)
    | Rmdir (path, delete_dir_name), Res ((Result (Unit,Exn),_), res) ->
      let full_path = path @ [delete_dir_name] in
      (match res with
       | Ok () ->
         Model.mem fs full_path && Model.path_is_a_dir fs full_path && path_is_an_empty_dir fs full_path
       | Error (Sys_error _) ->
         (not (Model.mem fs full_path)) ||
         (not (Model.path_is_a_dir fs full_path)) ||
         (not (path_is_an_empty_dir fs full_path))
       | Error _ -> false)
    | Readdir path, Res ((Result (Array String,Exn),_), res) ->
      (match res with
       | Ok array_of_subdir ->
         Model.mem fs path && Model.path_is_a_dir fs path &&
         (match Model.readdir fs path with
          | None   -> false
          | Some l ->
            List.sort String.compare l
            = List.sort String.compare (Array.to_list array_of_subdir))
       | Error (Sys_error _) ->
         (not (Model.mem fs path)) || (not (Model.path_is_a_dir fs path))
       | Error _ -> false)
    | Mkfile (path, new_file_name), Res ((Result (Unit,Exn),_),res) ->
      let full_path = path @ [new_file_name] in
      (match res with
       | Ok () -> Model.path_is_a_dir fs path && not (Model.mem fs full_path)
       | Error (Sys_error _) ->
         Model.mem fs full_path || (not (Model.mem fs path)) || (not (Model.path_is_a_dir fs path))
       | Error _ -> false)
    | _,_ -> false
end

module Sys_seq = STM_sequential.Make(SConf)
module Sys_dom = STM_domain.Make(SConf)

let rep_count = 50 (* No. of repetitions of the non-deterministic property *)
let retries = 10   (* Additional factor of repetition during shrinking *)
let _seq_len = 5    (* max length of the sequential prefix *)
let _par_len = 12   (* max length of the parallel cmd lists *)

let iteration = ref 0


let arb_triple =
  let triple =
    let open SConf in
    ([Mkdir ([], "hhh");
      Mkfile (["hhh"], "iii")],

     [Mkdir (["hhh"], "hhh");
      Mkdir (["hhh"; "iii"], "eee");
      Mkdir ([], "aaa");
      Rename (["hhh"; "hhh"], []);
      Mkfile (["hhh"; "hhh"], "ddd")],

     [Rename (["hhh"; "iii"], ["iii"; "ccc"]);
      Rmdir ([], "hhh");
      Mkdir (["hhh"], "bbb");
      Mkdir (["hhh"], "iii");
      Rmdir ([], "hhh")]) in
  make
    ~print:QCheck.Print.(triple (list SConf.show_cmd) (list SConf.show_cmd) (list SConf.show_cmd))
    (Gen.return triple)

let stress_test_par ~count ~name =
  Test.make ~retries ~count ~name
    arb_triple
    (fun triple ->
       Printf.printf "Iteration %i\n%!" !iteration;
       incr iteration;
       Printf.printf "%s\n\n%!"
         @@ Util.print_triple_vertical ~fig_indent:5 ~res_width:35 SConf.show_cmd triple;
       Util.repeat rep_count Sys_dom.stress_prop_par triple) (* 25 times each, then 25 * 10 times when shrinking *)

let _ =
  QCheck_base_runner.run_tests_main [
  (*Sys_seq.agree_test      ~count:1000 ~name:"STM Sys test sequential";*)
    stress_test_par ~count:1000 ~name:"STM Sys stress test parallel";
  ]
