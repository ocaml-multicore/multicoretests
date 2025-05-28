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

let protect (f : 'a -> 'b) (a : 'a) : ('b, exn) result =
  try Result.Ok (f a)
  with e -> Result.Error e

let rep_count = 50 (* No. of repetitions of the non-deterministic property *)

let stress_prop_par () =
  let sut = init_sut () in

  protect (fun () -> Sys.mkdir "_sandbox/hhh" 0o755) () |> ignore; (* file exists? *)
  protect (fun () -> mkfile "_sandbox/hhh/iii") () |> ignore;
  protect (fun () -> Sys.mkdir "_sandbox/hhh/hhh" 0o755) () |> ignore;

  let barrier = Atomic.make 2 in
  let dom1 () =
    Atomic.decr barrier;
    while Atomic.get barrier <> 0 do Domain.cpu_relax() done;
    Domain.cpu_relax();
    protect (fun () -> Sys.mkdir "_sandbox/hhh/iii/eee" 0o755) () |> ignore;
    Domain.cpu_relax();
    protect (fun () -> Sys.rename "_sandbox/hhh/hhh" "_sandbox") () |> ignore;
    Domain.cpu_relax();
    protect (fun () -> Sys.rename "_sandbox/bbb" "_sandbox") () |> ignore;
  in
  let dom2 () =
    Atomic.decr barrier;
    while Atomic.get barrier <> 0 do Domain.cpu_relax() done;
    protect (fun () -> Sys.rename "_sandbox/hhh/iii" "_sandbox/iii/ccc") () |> ignore;
    Domain.cpu_relax();
    protect (fun () -> Sys.rmdir "_sandbox/hhh") () |> ignore;
    Domain.cpu_relax();
    protect (fun () -> Sys.mkdir "_sandbox/hhh/iii" 0o755) () |> ignore;
    Domain.cpu_relax();
    protect (fun () -> Sys.rmdir "_sandbox/hhh") () |> ignore;
  in
  let dom1 = Domain.spawn dom1 in
  let dom2 = Domain.spawn dom2 in
  let _obs1 = Domain.join dom1 in
  let _obs2 = Domain.join dom2 in
  let ()   = cleanup sut in
  true

let rec repeat n prop input =
  n=0 || (prop input && repeat (n-1) prop input)

let _ =
  for i=1 to 1000 do
    Printf.printf "Iteration %i\n%!" i;
    repeat rep_count stress_prop_par () |> ignore (* 50 times each *)
  done
