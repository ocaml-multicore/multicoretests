open QCheck
open STM

module SConf = 
struct
  
  type cmd =
    | File_exists of string list 
    | Mkdir of string list * string * int
    | Rmdir of string list * string
    | Readdir of string list 
    [@@deriving show { with_path = false }]

  module Map_names = Map.Make (String) 

  type filesys = 
    | Directory of {perm: int; fs_map: filesys Map_names.t}
    | File of {perm: int}
  
  type state = filesys
  
  type sut   = unit 
  
  let (/) = Filename.concat

  let arb_cmd _s  =
    let name_gen = Gen.(oneofl ["aaa" ; "bbb" ; "ccc" ; "ddd" ; "eee"]) in
    let path_gen = Gen.map (fun path -> path) (Gen.list_size (Gen.int_bound 5) name_gen) in (* can be empty *)
    let perm_gen = Gen.(oneofl [0o777]) in
    (* let perm_gen = Gen.map3 (fun d1 d2 d3 -> d1*100 + d2*10 + d3*1) (Gen.int_bound 7) (Gen.int_bound 7) (Gen.int_bound 7) in *)
    QCheck.make ~print:show_cmd 
      Gen.(oneof
            [ 
                map (fun path -> File_exists (path)) path_gen ;
                map3 (fun path new_dir_name perm -> Mkdir (path, new_dir_name, perm)) path_gen name_gen perm_gen;
                map2  (fun path delete_dir_name -> Rmdir (path, delete_dir_name)) path_gen name_gen;
                map (fun path -> Readdir (path)) path_gen;
            ])

  let static_path = Sys.getcwd ()

  let init_state  = Directory {perm = 0o777; fs_map = Map_names.empty}

  (* val find_opt : filesys -> string list -> filesys option *)
  let rec find_opt fs path = 
    match fs with
    | File f -> if path = [] then Some (File f) else None
    | Directory d -> 
      (match path with 
      | []       -> Some (Directory d)
      | hd :: tl -> 
        (match Map_names.find_opt hd d.fs_map with
        | None    -> None
        | Some fs -> find_opt fs tl))

  (* val mem : filesys -> string list -> bool *)
  let mem fs path = 
    match find_opt fs path with
    | None   -> false
    | Some _ -> true

  (* val mkdir : filesys -> string list -> string -> int -> filesys *)
  let rec mkdir fs path new_dir_name perm = 
    match fs with
    | File _      -> fs
    | Directory d -> 
      (match path with 
      | [] ->  
        let new_dir = Directory {perm; fs_map = Map_names.empty} in
        Directory {d with fs_map = Map_names.add new_dir_name new_dir d.fs_map}
      | next_in_path :: tl_path -> 
        (match Map_names.find_opt next_in_path d.fs_map with
        | None        -> fs
        | Some sub_fs -> 
          let nfs = mkdir sub_fs tl_path new_dir_name perm in
          if nfs = sub_fs
          then fs
          else 
            let new_map = Map_names.remove next_in_path d.fs_map in
            let new_map = Map_names.add next_in_path nfs new_map in
            Directory {d with fs_map = new_map}))

  (* val readdir : filesys -> string list -> string list option *)
  let readdir fs path = 
    match find_opt fs path with
    | None    -> None
    | Some fs -> 
      match fs with
      | File _ -> None
      | Directory d -> Some (Map_names.fold (fun k _ l -> l @ [k]) d.fs_map [])


  (* val rmdir : filesys -> string list -> string -> filesys *)
  let rec rmdir fs path delete_dir_name = 
    match fs with
    | File _      -> fs
    | Directory d -> 
      (match path with 
      | [] ->  
        let target_fs = Map_names.find_opt delete_dir_name d.fs_map in
        (match target_fs with
        | None -> Directory d
        | Some fs ->
          (match fs with
          | File _ -> Directory d
          | Directory target -> 
            let readd_target = (Map_names.fold (fun k _ l -> l @ [k]) target.fs_map []) in
            if readd_target = []
            then Directory {d with fs_map = Map_names.remove delete_dir_name d.fs_map}
            else Directory d))
      | next_in_path :: tl_path -> 
        (match Map_names.find_opt next_in_path d.fs_map with
        | None        -> fs
        | Some sub_fs -> 
          let nfs = rmdir sub_fs tl_path delete_dir_name in
          if nfs = sub_fs
          then fs
          else 
            let new_map = Map_names.remove next_in_path d.fs_map in
            let new_map = Map_names.add next_in_path nfs new_map in
            Directory {d with fs_map = new_map}))

  let next_state c fs = 
    match c with
    | File_exists (_path) -> fs
    | Mkdir (path, new_dir_name, perm) -> 
      if mem fs (path @ [new_dir_name])
      then fs
      else mkdir fs path new_dir_name perm
    | Rmdir (path,delete_dir_name) ->
      if mem fs (path @ [delete_dir_name])
      then rmdir fs path delete_dir_name
      else fs
    | Readdir _path -> fs 

  let init_sut () = try Sys.mkdir (static_path / "sandbox_root") 0o777 with Sys_error _ -> ()
    (* ignore (Sys.command ("mkdir " ^ static_path ^ "/sandbox_root"))  *)
    (* creer sandbox_root
      si il existe deja   
        il est vide       = on fait rien
        il est pas vide   = on le rm -d et on le recree
      si il existe pas    = on le cree *)
      (* let path = static_path ^ "/sandbox_root" in
    ignore (Sys.command ("rm -rf " ^ path ^ " && mkdir " ^ path))  *)
    


    (* let err = Sys.command ("mkdir " ^ static_path ^ "/sandbox_root") in
    if err = 1
    then 
      (Format.printf "then\n";
       ignore (Sys.command ("rm -r -d -f " ^ static_path ^ "/sandbox_root"));
       ignore (Sys.command ("mkdir " ^ static_path ^ "/sandbox_root")))
    else Format.printf "else\n" *)

  let cleanup _   = ignore (Sys.command ("rm -r -d -f " ^ static_path ^ "/sandbox_root"))

  let precond _c _s = true 

  let run c _file_name = match c with
    | File_exists (path) -> Res (bool, Sys.file_exists (static_path / "sandbox_root" / (List.fold_left (/) "" path)))
    | Mkdir (path, new_dir_name, perm) -> 
      Res (result unit exn, protect (Sys.mkdir (static_path / "sandbox_root" / (List.fold_left (/) "" path) / new_dir_name))perm)
    | Rmdir (path, delete_dir_name) -> 
      Res (result unit exn, protect (Sys.rmdir) (static_path / "sandbox_root" / (List.fold_left (/) "" path) / delete_dir_name))
    | Readdir (path) -> 
      Res (result (array string) exn, protect (Sys.readdir) (static_path / "sandbox_root" / (List.fold_left (/) "" path)))

  let cut_path p = List.rev (List.tl (List.rev p)) 
  let is_a_dir fs = match fs with | Directory _ -> true | File _ -> false

  let postcond c (fs: filesys) res = 
    let p path = static_path / "sandbox_root" / (String.concat "/" path) in
    match c, res with
    | File_exists (path), Res ((Bool,_),b) -> b = mem fs path
    | Mkdir (path, new_dir_name, _perm), Res ((Result (Unit,Exn),_), Error (Sys_error (s) ))
      when s = (p (path @ [new_dir_name])) ^ ": Permission denied"         -> true
    | Mkdir (path, new_dir_name, _perm), Res ((Result (Unit,Exn),_), Error (Sys_error (s) ))
      when s = (p (path @ [new_dir_name])) ^ ": File exists"               -> 
      mem fs (path @ [new_dir_name]) 
    | Mkdir (path, new_dir_name, _perm), Res ((Result (Unit,Exn),_), Error (Sys_error (s) ))
      when s = (p (path @ [new_dir_name])) ^ ": No such file or directory" -> 
      let b = not (mem fs path) in
      b
    | Mkdir (path, new_dir_name, _perm), Res ((Result (Unit,Exn),_), Error (Sys_error (s) ))
      when s = (p (path @ [new_dir_name])) ^ ": Not a directory" ->
      (match find_opt fs (path @ [new_dir_name]) with
      | None -> false
      | Some target_fs -> not (is_a_dir target_fs))
    | Mkdir (path, new_dir_name, _perm), Res ((Result (Unit,Exn),_), Ok ()) -> 
      let is_existing_is_a_dir =
        (match find_opt fs path with
        | None -> false 
        | Some target_fs -> is_a_dir target_fs) in
      true && not (mem fs (path @ [new_dir_name])) && mem fs path && is_existing_is_a_dir
      | Mkdir (_path, _new_dir_name, _perm), Res ((Result (Unit,Exn),_), _) -> false



    | Rmdir (path, delete_dir_name), Res ((Result (Unit,Exn),_), Error (Sys_error (s) ))
      when s = (p (path @ [delete_dir_name])) ^ ": No such file or directory" -> 
      not (mem fs (path @ [delete_dir_name]))

    | Rmdir (path, delete_dir_name), Res ((Result (Unit,Exn),_), Error (Sys_error (s) ))
      when s = (p (path @ [delete_dir_name])) ^ ": Directory not empty" ->
      not (readdir fs (path @ [delete_dir_name]) = Some [])


    | Rmdir (path, delete_dir_name), Res ((Result (Unit,Exn),_), Error (Sys_error (s) ))
      when s = (p (path @ [delete_dir_name])) ^ ": Not a directory" -> 
      (match find_opt fs (path @ [delete_dir_name]) with
      | None -> false
      | Some target_fs -> not (is_a_dir target_fs))


    | Rmdir (path, delete_dir_name), Res ((Result (Unit,Exn),_), Ok ()) -> 
      let is_empty = readdir fs (path @ [delete_dir_name]) = Some [] in
      let is_a_dir =
        (match find_opt fs (path @ [delete_dir_name]) with
        | None -> false
        | Some target_fs -> is_a_dir target_fs) in 
      mem fs (path @ [delete_dir_name]) && is_empty && is_a_dir

    | Rmdir (_path, _delete_dir_name), Res ((Result (Unit,Exn),_), _r) -> false



    | Readdir (path), Res ((Result (Array String,Exn),_), Error (Sys_error (s) ))
      when s = (p path) ^ ": Permission denied" -> true
    | Readdir (path), Res ((Result (Array String,Exn),_), Error (Sys_error (s) ))
      when s = (p path) ^ ": No such file or directory" ->
      not (mem fs path)
    | Readdir (path), Res ((Result (Array String,Exn),_), Error (Sys_error (s) ))
      when s = (p path) ^ ": Not a directory" ->
      (* not (is_a_dir fs path) *)
      (match find_opt fs path with
      | None -> false
      | Some target_fs -> is_a_dir target_fs) 
    | Readdir (path), Res ((Result (Array String,Exn),_), r) -> 
     (match r with
      | Error _e -> false
      | Ok a     -> 
        let sut   = List.sort (fun a b -> -(String.compare a b)) (Array.to_list a) in
        (match readdir fs path with
          | None   -> false
          | Some l -> List.sort (fun a b -> -(String.compare a b)) l = sut  )  )




    | _,_ -> false
end

module SysSTM = STM.Make(SConf)

;;
Util.set_ci_printing ()
;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [SysSTM.agree_test         ~count ~name:"STM Sys test sequential";
   (* SysSTM.neg_agree_test_par ~count ~name:"STM Sys test parallel" *)
])
