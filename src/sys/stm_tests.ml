open QCheck
open STM

module SConf = 
struct
  
  type cmd =
    | File_exists of string list * string
    | Mkdir of string list * string * int
    | Rmdir of string list * string  
    | Readdir of string list * string
    [@@deriving show { with_path = false }]

  type filesys = 
    | Directory of {
        perm: int;
        dir_name: string;
        fs_list: filesys list }
    | File of {
        perm: int;
        file_name: string }
    [@@deriving show { with_path = false }]
  
  type state = filesys
  
  type sut   = unit 
  
  let (/) = Filename.concat

  let arb_cmd _s  =
    let str_gen  = Gen.(oneofl ["c"; "e"; "r"]) in
    let name_gen = Gen.(oneofl ["aaa" ; "bbb" ; "ccc" ; "ddd" ; "eee"]) in
    let path_gen = Gen.map (fun path -> "sandbox_root" :: path) (Gen.list_size (Gen.int_bound 5) name_gen) in
    let perm_gen = Gen.(oneofl [0o777]) in
    (* let perm_gen = Gen.map3 (fun d1 d2 d3 -> d1*100 + d2*10 + d3*1) (Gen.int_bound 7) (Gen.int_bound 7) (Gen.int_bound 7) in *)
    QCheck.make ~print:show_cmd 
      Gen.(oneof
            [ 
                map2 (fun path name -> File_exists (path, name)) path_gen str_gen;
                map3 (fun path dir_name perm -> Mkdir (path, dir_name, perm)) path_gen str_gen perm_gen;
                map2 (fun path dir_name -> Rmdir (path, dir_name)) path_gen str_gen;
                map2 (fun path dir_name -> Readdir (path, dir_name)) path_gen str_gen;
            ])

  let static_path = Sys.getcwd ()

  let init_state  = 
    Directory {perm = 0o777; dir_name = "sandbox_root"; fs_list = []}

  let rec is_perm_ok (fsl: filesys list) path = 
    match fsl with
    | [] -> true 
    | Directory d :: tl -> (match path with
      | [] -> true
      | hd_path :: tl_path -> if d.dir_name = hd_path
        then if d.perm > 447
             then is_perm_ok d.fs_list tl_path
             else false
        else is_perm_ok tl path
      )
    | File f :: tl -> (match path with
      | [] -> true
      | hd_path :: _tl_path -> if f.file_name = hd_path
        then true
        else is_perm_ok tl path
      )

  let rec mem fs path name = 
    match fs with 
    | File f      -> path = [] && f.file_name = name 
    | Directory d -> 
      match path with
      | []       -> name = d.dir_name
      | hd :: tl -> 
        if hd = d.dir_name
        then List.exists (fun f -> mem f tl name) d.fs_list 
        else false

  let is_dir_empty = function
    | File _f -> false
    | Directory d -> d.fs_list = []

  let is_a_dir = function
    | File _f -> false
    | Directory _d -> true

  let rec is_path_empty fs path dir_name = 
    match fs with 
    | File _f      -> false
    | Directory d -> 
      match path with
      | []       -> dir_name = d.dir_name && is_dir_empty (Directory d)
      | hd :: tl -> 
        if hd = d.dir_name
        then List.exists (fun f -> is_path_empty f tl dir_name) d.fs_list 
        else false
  
  let rec is_path_a_dir fs path dir_name = 
    match fs with 
    | File _f      -> false
    | Directory d -> 
      match path with
      | []       -> dir_name = d.dir_name
      | hd :: tl -> 
        if hd = d.dir_name
        then List.exists (fun f -> is_path_a_dir f tl dir_name) d.fs_list 
        else false

  let get_name fs =
    match fs with
    | File f -> f.file_name
    | Directory d -> d.dir_name

  let rec mkdir fs path dir_name perm = 
    match fs with 
    | File _f     -> fs 
    | Directory d -> 
      match path with 
      | []       -> 
        if List.exists (fun fs -> get_name fs = dir_name) d.fs_list
        then fs
        else Directory {d with fs_list = (Directory {perm; dir_name; fs_list = []} :: d.fs_list)}
      | hd :: [] ->
        if hd = d.dir_name
        then (
          if List.exists (fun fs -> get_name fs = dir_name) d.fs_list
          then fs
          else Directory {d with fs_list = (Directory {perm; dir_name; fs_list = []} :: d.fs_list)})
        else fs
      | hd :: tl -> 
        if hd = d.dir_name
        then Directory {d with fs_list = List.map (fun f -> mkdir f tl dir_name perm) d.fs_list}
        else fs

  let rec rmdir fs path dir_name = 
    match fs with 
    | File _f     -> fs 
    | Directory d -> 
      match path with 
      | []       -> 
        if not (List.exists (fun fs -> get_name fs = dir_name) d.fs_list) || not (is_dir_empty (Directory d))
        then fs
        else Directory {d with fs_list = List.filter (fun f -> not (get_name f = dir_name)) d.fs_list}
      | hd :: [] ->
        if List.exists (fun fs -> get_name fs = dir_name && is_dir_empty fs) d.fs_list 
           && not (is_dir_empty (Directory d)) 
           && hd = d.dir_name
        then Directory {d with fs_list = List.filter (fun f -> not (get_name f = dir_name)) d.fs_list}
        else fs
      | hd :: tl ->
        if not (hd = d.dir_name)
        then fs
        else Directory {d with fs_list = List.map (fun f -> rmdir f tl dir_name) d.fs_list}

  let rec find_opt fs path name = 
    match fs with
    | File _f -> None
    | Directory d ->
        match path with
        | [] -> 
          if d.dir_name = name
          then Some (Directory d)
          else None
        | hd :: tl -> 
          if d.dir_name = hd
          then (
            let path_next = (match tl with | [] -> name | _ :: _ -> List.hd tl) in
            let fs_next = List.find_opt (
              fun e -> 
                match e with
                | File _f -> false
                | Directory d -> d.dir_name = path_next) d.fs_list in
            match fs_next with
            | None -> None 
            | Some fs -> find_opt fs tl name)
          else None

  let get_sub_name fs = 
    match fs with
    | File f      -> f.file_name
    | Directory d -> d.dir_name

  let readdir fs path dir_name = 
    match find_opt fs path dir_name with
    | None   -> None
    | Some fs -> 
      match fs with
      | File _f -> None
      | Directory d -> Some (List.map get_sub_name d.fs_list)

  let next_state c fs = 
    match c with
    | File_exists (_path, _name)   -> fs
    | Mkdir (path, dir_name, perm) -> 
      if mem fs path dir_name
      then fs
      else mkdir fs path dir_name perm
    | Rmdir (path, dir_name) ->
      if mem fs path dir_name
      then rmdir fs path dir_name
      else fs
    | Readdir (_path, _dir_name) -> fs 

  let init_sut () = try Sys.mkdir (static_path / "sandbox_root") 0o777 with Sys_error _ -> ()

  let cleanup _   = ignore (Sys.command ("rm -r -d -f " ^ (static_path / "sandbox_root")))

  let precond _c _s = true 

  let run c _file_name = match c with
    | File_exists (path, name) -> Res (bool, Sys.file_exists (static_path / (List.fold_left (/) "" path) / name))
    | Mkdir (path, dir_name, perm) -> 
      Res (result unit exn, protect (Sys.mkdir (static_path / (List.fold_left (/) "" path) / dir_name))perm)
    | Rmdir (path, dir_name) -> 
      Res (result unit exn, protect (Sys.rmdir) (static_path / (List.fold_left (/) "" path) / dir_name))
    | Readdir (path, dir_name) -> 
      Res (result (array string) exn, protect (Sys.readdir) (static_path / (List.fold_left (/) "" path) / dir_name))

  let postcond c (fs: filesys) res = 
    let p path dir_name = static_path / (String.concat "/" path) / dir_name in
    match c, res with
    | File_exists (path, name), Res ((Bool,_),b) -> b = mem fs path name
    | Mkdir (path, dir_name, _perm), Res ((Result (Unit,Exn),_), Error (Sys_error (s) ))
      when s = (p path dir_name) ^ ": Permission denied"         -> 
      not (is_perm_ok [fs] path)
    | Mkdir (path, dir_name, _perm), Res ((Result (Unit,Exn),_), Error (Sys_error (s) ))
      when s = (p path dir_name) ^ ": File exists"               -> 
      mem fs path dir_name 
    | Mkdir (path, dir_name, _perm), Res ((Result (Unit,Exn),_), Error (Sys_error (s) ))
      when s = (p path dir_name) ^ ": No such file or directory" -> 
      (match path with
      | [] -> false
      | _hd_path :: _tl_path -> 
        let rev = List.rev path in
        not (mem fs (List.rev (List.tl rev)) (List.hd rev)))
    | Mkdir (path, dir_name, _perm), Res ((Result (Unit,Exn),_), Ok ()) -> 
      let b = (
        match path with
        | [] -> true
        | _hd_path :: _tl_path -> 
          let rev = List.rev path in
          mem fs (List.rev (List.tl rev)) (List.hd rev)) in
      is_perm_ok [fs] path && not (mem fs path dir_name) && b
    | Mkdir (_path, _dir_name, _perm), Res ((Result (Unit,Exn),_), _r) -> false
    | Rmdir (path, dir_name), Res ((Result (Unit,Exn),_), Error (Sys_error (s) ))
      when s = (p path dir_name) ^ ": No such file or directory" -> 
      not (mem fs path dir_name)
    | Rmdir (path, dir_name), Res ((Result (Unit,Exn),_), Error (Sys_error (s) ))
      when s = (p path dir_name) ^ ": Directory not empty" -> 
      not (is_path_empty fs path dir_name)
    | Rmdir (path, dir_name), Res ((Result (Unit,Exn),_), Error (Sys_error (s) ))
      when s = (p path dir_name) ^ ": Not a directory" ->
      not (is_path_a_dir fs path dir_name)
    | Rmdir (path, dir_name), Res ((Result (Unit,Exn),_), Ok ()) -> 
      mem fs path dir_name  && is_path_empty fs path dir_name
    | Rmdir (_path, _dir_name), Res ((Result (Unit,Exn),_), _r) -> false
    | Readdir (path, dir_name), Res ((Result (Array String,Exn),_), Error (Sys_error (s) ))
      when s = (p path dir_name) ^ ": No such file or directory" ->
      not (mem fs path dir_name)
    | Readdir (path, dir_name), Res ((Result (Array String,Exn),_), Error (Sys_error (s) ))
      when s = (p path dir_name) ^ ": Not a directory" ->
      not (is_path_a_dir fs path dir_name)
    | Readdir (path, dir_name), Res ((Result (Array String,Exn),_), r) -> 
     (match r with
      | Error _e -> false
      | Ok a     -> 
        let sut   = List.sort (fun a b -> -(String.compare a b)) (Array.to_list a) in
        (match readdir fs path dir_name with
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
