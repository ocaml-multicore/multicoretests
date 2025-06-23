(*
                                       |                   
                                Mkdir ([], hhh)            
                              Mkfile ([hhh], iii)          
                              Mkdir ([hhh], hhh)           
                                       |                   
                    .------------------------------------.
                    |                                    |                   
         Mkdir ([hhh; iii], eee)          Rename ([hhh; iii], [iii; ccc])    
         Rename ([hhh; hhh], [])                  Rmdir ([], hhh)            
           Rename ([bbb], [])                   Mkdir ([hhh], iii)           
                                                  Rmdir ([], hhh)            
*)

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

let mkfile filepath =
  let flags = [Open_wronly; Open_creat; Open_excl] in
  Out_channel.with_open_gen flags 0o666 filepath (fun _ -> ())

let stress_prop_par () =
  let sut = init_sut () in

  Sys.mkdir "_sandbox/hhh" 0o755;
  mkfile "_sandbox/hhh/iii";
  Sys.mkdir "_sandbox/hhh/hhh" 0o755;

  let barrier = Atomic.make 2 in
  let dom1 () =
    Atomic.decr barrier;
    while Atomic.get barrier <> 0 do Domain.cpu_relax() done;
    (try Sys.rename "_sandbox/hhh/hhh" "_sandbox"; assert false with _ -> ());
    (try Sys.rename "_sandbox/bbb" "_sandbox"; assert false with _ -> ());
    (try Sys.mkdir "_sandbox/hhh/iii/eee" 0o755; assert false with _ -> ());
  in
  let dom2 () =
    Atomic.decr barrier;
    while Atomic.get barrier <> 0 do Domain.cpu_relax() done;
    (try Sys.rename "_sandbox/hhh/iii" "_sandbox/iii/ccc"; assert false with _ -> ());
    (try Sys.mkdir "_sandbox/hhh/iii" 0o755; assert false with _ -> ());
    (try Sys.rmdir "_sandbox/hhh"; assert false with _ -> ());
    (try Sys.rmdir "_sandbox/hhh"; assert false with _ -> ());
  in
  let dom1 = Domain.spawn dom1 in
  let dom2 = Domain.spawn dom2 in
  Domain.join dom1;
  Domain.join dom2;
  cleanup sut

let rec repeat n prop input =
  if n=0 then () else (prop input; Printf.printf "#%!"; repeat (n-1) prop input)

let _ =
  let rep_count = 50 in (* No. of inner repetitions of the non-deterministic property *)
  for i=1 to 1000 do
    Printf.printf "\nIteration %i %!" i;
    Sys.(signal sigalrm (Signal_handle (fun _ -> (Obj.magic ()).(10)))) |> ignore;
    ignore (Unix.alarm 3);
    repeat rep_count stress_prop_par (); (* 50 times each *)
    ignore (Unix.alarm 0) (*cancel alarm*)
  done
