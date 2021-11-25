(** Experimental module to help test non-deterministic/concurrent code
    by repeating the tested property during shrinking (Hughes-Bolinder:Erlang11) *)

(* [Non_det.Test] mirrors [Test], roughly *)
module Test =
struct
  exception Done
  let rec make ?(repeat=250) ?count ?name arb law =
    let law_non_det x =
      if Atomic.get shrinking
      then
        try
          for _i=1 to repeat do
            if not (law x) then raise Done
          done;
          true
        with Done -> false
      else
        law x in
    QCheck2.Test.Test (QCheck.Test.make_cell ?count ?name arb law_non_det)

  (* hack: an atomic to mirror state of underlying system *)
  (*       only the main 'domain' w/the runner touches it *)
  and shrinking = Atomic.make false

  (* Test handler to capture the switch between the generation and shrinking stages *)
  and handler _ _ = function
    | QCheck2.Test.Generating  -> Atomic.set shrinking false(*; Printf.printf "handler - generating\n%!"*)
    | QCheck2.Test.Testing _   -> Atomic.set shrinking false(*; Printf.printf "handler - testing\n%!"*)
    | QCheck2.Test.Shrunk _    -> Atomic.set shrinking true(*; Printf.printf "handler - shrunk\n%!"*)
    | QCheck2.Test.Shrinking _ -> Atomic.set shrinking true(*; Printf.printf "handler - shrinking\n%!"*)
    | QCheck2.Test.Collecting _-> ()

  and check_cell_exn ?rand cell =
    let res = QCheck2.Test.check_cell ~handler ?rand cell in
    QCheck2.Test.check_result cell res

  let check_exn ?rand (QCheck2.Test.Test cell) = check_cell_exn ?rand cell
end

(* [Non_det.QCheck_runner] mirrors [QCheck_runner], roughly *)
module QCheck_runner =
struct
  (* Runner handler *)
  let handler_gen ~colors ~debug_shrink ~debug_shrink_list ~size ~out ~verbose c =
    let default_handler =
      QCheck_base_runner.default_handler ~colors ~debug_shrink ~debug_shrink_list ~size ~out ~verbose c in
    let handler name cell r =
       default_handler.handler name cell r;
       Test.handler name cell r in
    { QCheck_base_runner.handler = handler }

  let run_tests ?colors ?verbose ?long ?out ?rand ts =
    QCheck_base_runner.run_tests ~handler:handler_gen
      ?colors ?verbose ?long ?out ?rand ts


let run_tests_main ?(argv=Sys.argv) tests =
  try
    let cli_args = QCheck_base_runner.Raw.parse_cli ~full_options:false argv in
    exit
      (run_tests tests
         ~colors:cli_args.cli_colors
         ~verbose:cli_args.cli_verbose
         ~long:cli_args.cli_long_tests ~out:stdout ~rand:cli_args.cli_rand)
  with
    | Arg.Bad msg -> print_endline msg; exit 1
    | Arg.Help msg -> print_endline msg; exit 0
end

(* example:

let t = Non_det.Test.make (int_bound 1000) (fun n -> Printf.printf "%i.%!" n; 1 = n mod 4);;
Non_det.Test.check t;;

Non_det.Qcheck_runner.run_tests
*)
