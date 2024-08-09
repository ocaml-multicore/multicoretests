let test () =
  let path = Filename.temp_file "stm-" "" in
  let channel = Atomic.make (Out_channel.open_text path) in

  (* First, a bit of one-domain channel activity *)
  Out_channel.output_byte (Atomic.get channel) 1;
  (try Out_channel.flush (Atomic.get channel) with (Sys_error _) -> assert false);

  let wait = Atomic.make true in

  (* Domain 1 closes-opens-outputs repeatedly *)
  let d1 = Domain.spawn (fun () ->
      while Atomic.get wait do Domain.cpu_relax() done;
      for _ = 1 to 50 do
        Out_channel.close (Atomic.get channel);
        Atomic.set channel (Out_channel.open_text path);
        Out_channel.output_byte (Atomic.get channel) 1;
      done;
    ) in

  (* Domain 2 calls flush and pos repeatedly *)
  let d2 = Domain.spawn (fun () ->
      Atomic.set wait false;
      (*
       "Output functions raise a Sys_error exception when they are applied to a  closed  output  channel,
        except  Out_channel.close  and Out_channel.flush , which do nothing when applied to an already closed channel."
      *)
      for _ = 1 to 50 do
        (try Out_channel.flush (Atomic.get channel)
         with (Sys_error msg) -> Printf.printf "Out_channel.flush raised Sys_error %S\n%!" msg; assert false);
        ignore (Out_channel.pos (Atomic.get channel));
      done;
    ) in

  let () = Domain.join d1 in
  let () = Domain.join d2 in
  (* Please leave the torture chamber nice and clean as you found it *)
  (try Out_channel.close (Atomic.get channel) with Sys_error _ -> ());
  Sys.remove path

let _ =
  for i = 1 to 5_000 do
    if i mod 250 = 0 then Printf.printf "#%!";
    test ()
  done
