let length = 8

type cmd = Get of int

let init_sut () = List.init length (fun i -> Domain.DLS.new_key (fun () -> i))

let rec interp sut cs = match cs with
  | [] -> ()
  | (Get i)::cs ->
    assert (i = Domain.DLS.get (List.nth sut i));
    interp sut cs

let run n =
  for i = 1 to n do
    if i mod 100 = 0 then Printf.printf "#%!";
    let cmds = List.init 30 (fun _ -> Get (Random.int length)) in
    (*[Get 0; Get 1; Get 2; Get 3; Get 7; Get 6; Get 5; Get 4; ]*)
    Domain.spawn (fun () ->
        let sut = init_sut () in
        interp sut cmds) |> Domain.join
  done

let _ =
  Random.self_init ();
  run 50_000
