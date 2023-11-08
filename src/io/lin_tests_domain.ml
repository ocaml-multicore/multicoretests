(* ********************************************************************** *)
(*                      Tests of in and out channels                      *)
(* ********************************************************************** *)

module IC_domain = Lin_domain.Make(Lin_tests_spec_io.ICConf)
module OC_domain = Lin_domain.Make(Lin_tests_spec_io.OCConf)

let tests =
  IC_domain.neg_lin_test ~count:1000 ~name:"Lin In_channel test with Domain" ::
  if Sys.getenv_opt "OCAML_SYSTEM" = Some "macosx"
  then (
    Printf.printf "Lin Out_channel test with Domain disabled under macOS\n\n%!";
    []
  ) else [
    OC_domain.neg_lin_test ~count:5000 ~name:"Lin Out_channel test with Domain";
  ]

let _ = QCheck_base_runner.run_tests_main tests
