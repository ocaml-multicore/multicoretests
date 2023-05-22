open QCheck
open STM

(* Test file system bindings *)
module UConf =
struct
  type path = string list

  type cmd =
    | Rename of (path * string) * (path * string)
    | Mkdir of path * string
    | Rmdir of path * string

  let pp_cmd par fmt x =
    let open Util.Pp in
    let pp_path = pp_list pp_string in
    match x with
    | Rename ((xp, xn), (yp, yn)) ->
      Format.fprintf fmt "Rename (%a,%a) (%a,%a)" (pp_path false) xp (pp_string false) xn (pp_path false) yp (pp_string false) yn
    | Mkdir (x, y) -> cst2 pp_path pp_string "Mkdir" par fmt x y
    | Rmdir (x, y) -> cst2 pp_path pp_string "Rmdir" par fmt x y

  let show_cmd = Util.Pp.to_show pp_cmd

  module Map_names = Map.Make (String)

  type filesys =
    | Directory of {fs_map: filesys Map_names.t}
    | File

  type state = filesys

  type sut = unit

  let (/) = Filename.concat

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
  let _path_gen s = Gen.(oneof [gen_existing_path s; list_size (int_bound 5) name_gen]) (* can be empty *)
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
          map2 (fun src dest -> Rename (src, dest)) (pair_gen s) (pair_gen s);
          map (fun (path,new_dir_name) -> Mkdir (path, new_dir_name)) (pair_gen s);
          map (fun (path,delete_dir_name) -> Rmdir (path, delete_dir_name)) (pair_gen s);
        ])

  let sandbox_root = "_sandbox"

  let init_state  = Directory {fs_map = Map_names.empty}

  module Model =
  struct
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

    let rec mkdir fs path new_dir_name =
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
              let nfs = mkdir sub_fs tl_path new_dir_name in
              if nfs = sub_fs
              then fs
              else
                let new_map = Map_names.remove next_in_path d.fs_map in
                let new_map = Map_names.add next_in_path nfs new_map in
                Directory {fs_map = new_map}))

    let readdir fs path =
      match find_opt fs path with
      | None    -> None
      | Some fs ->
        (match fs with
         | File -> None
         | Directory d -> Some (Map_names.fold (fun k _ l -> k::l) d.fs_map []))

    let update_map_name map_name k v = Map_names.update k (fun _ -> Some v) map_name

    let rec rmdir fs path delete_dir_name =
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
              let nfs = rmdir sub_fs tl_path delete_dir_name in
              if nfs = sub_fs
              then fs
              else Directory {fs_map = (update_map_name d.fs_map next_in_path nfs)}))

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

    let rename fs (old_path_pref, old_name) (new_path_pref, new_name) = match find_opt fs new_path_pref with
      | None
      | Some File -> fs
      | Some (Directory _) ->
        (match find_opt fs (old_path_pref@[old_name]) with
         | None -> fs
         | Some sub_fs ->
           let fs' = remove fs old_path_pref old_name in
           insert fs' new_path_pref new_name sub_fs)
  end

  let rec is_true_prefix path1 path2 = match path1, path2 with
    | [], [] -> false
    | [], _::_ -> true
    | _::_, [] -> false
    | n1::p1, n2::p2 -> n1=n2 && is_true_prefix p1 p2

  let next_state c fs =
    match c with
    | Rename ((src_path,src_name), (dest_path,dest_name)) ->
      (* FIXME: only for dirs for now *)
      if is_true_prefix (src_path@[src_name]) (dest_path@[dest_name])
      then fs
      else
      if (not (Model.mem fs (dest_path@[dest_name])) || Model.readdir fs (dest_path@[dest_name]) = Some [])
      then
        Model.rename fs (src_path,src_name) (dest_path,dest_name)
      else fs
    | Mkdir (path, new_dir_name) ->
      if Model.mem fs (path @ [new_dir_name])
      then fs
      else Model.mkdir fs path new_dir_name
    | Rmdir (path,delete_dir_name) ->
      if Model.mem fs (path @ [delete_dir_name])
      then Model.rmdir fs path delete_dir_name
      else fs

  let init_sut () =
    try Unix.mkdir sandbox_root 0o700
    with Unix.Unix_error(Unix.EEXIST, "mkdir", "_sandbox") -> ()

  let cleanup _ =
    match Sys.os_type with
    | "Cygwin"
    | "Unix"  -> ignore (Sys.command ("rm -r " ^ Filename.quote sandbox_root))
    | "Win32" -> ignore (Sys.command ("rd /s /q " ^ Filename.quote sandbox_root))
    | v -> failwith ("Sys tests not working with " ^ v)

  let precond _c _s = true

  let p path =  (List.fold_left (/) sandbox_root path)

  let run c _file_name =
    match c with
    | Rename ((src_path,src_name), (dest_path,dest_name)) ->
      Res (result unit exn, protect (Unix.rename ((p src_path) / src_name)) ((p dest_path) / dest_name))
    | Mkdir (path, new_dir_name) ->
      Res (result unit exn, protect (Unix.mkdir ((p path) / new_dir_name)) 0o755)
    | Rmdir (path, delete_dir_name) ->
      Res (result unit exn, protect (Unix.rmdir) ((p path) / delete_dir_name))

  let path_is_a_dir fs path =
    match Model.find_opt fs path with
    | None -> false
    | Some (Directory _) -> true
    | Some File -> false

  let postcond c (fs: filesys) res =
    match c, res with
    | Rename ((src_path,src_name),(dest_path,dest_name)), Res ((Result (Unit,Exn),_), res) ->
      (match res with
       | Error (Unix.Unix_error (err, "rename", _error_path)) ->
         (match err with
          | Unix.ENOENT    -> not (Model.mem fs (src_path@[src_name])) || not (Model.mem fs dest_path)
          | Unix.EINVAL    -> is_true_prefix (src_path@[src_name]) (dest_path@[dest_name])
          | Unix.ENOTEMPTY -> is_true_prefix (dest_path@[dest_name]) (src_path@[src_name]) ||
                              (path_is_a_dir fs (dest_path@[dest_name]) && not (Model.readdir fs (dest_path@[dest_name]) = Some []))
          | _ -> false)
       | Error _ -> false
       | Ok () -> true)
    | Mkdir (path, new_dir_name), Res ((Result (Unit,Exn),_), res) ->
      let complete_path = path @ [new_dir_name] in
      (match res with
       | Error (Unix.Unix_error (err, "mkdir", error_path)) ->
         (match err with
          | Unix.EEXIST -> Model.mem fs complete_path && p complete_path = error_path
          | Unix.ENOENT -> not (Model.mem fs path) && p complete_path = error_path
          | _ -> false)
       | Error _ -> false
       | Ok () -> Model.mem fs path && path_is_a_dir fs path && not (Model.mem fs complete_path))
    | Rmdir (path, delete_dir_name), Res ((Result (Unit,Exn),_), res) ->
      let complete_path = path @ [delete_dir_name] in
      (match res with
       | Error (Unix.Unix_error (err, "rmdir", error_path)) ->
         (match err with
          | Unix.ENOENT    -> not (Model.mem fs complete_path) && p complete_path = error_path
          | Unix.ENOTEMPTY -> not (Model.readdir fs complete_path = Some []) && p complete_path = error_path
          | _ -> false)
       | Error _ -> false
       | Ok () ->
          Model.mem fs complete_path && path_is_a_dir fs complete_path && Model.readdir fs complete_path = Some [])
    | _,_ -> false
end


module Unix_seq = STM_sequential.Make(UConf)
module Unix_dom = STM_domain.Make(UConf)

;;
QCheck_base_runner.run_tests_main [
    Unix_seq.agree_test         ~count:1000 ~name:"STM Unix test sequential";
    Unix_dom.neg_agree_test_par ~count:1000 ~name:"STM Unix test parallel"
  ]
