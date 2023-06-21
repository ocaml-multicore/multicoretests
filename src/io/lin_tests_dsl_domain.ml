(* ********************************************************************** *)
(*                      Tests of in and out channels                      *)
(* ********************************************************************** *)

open Lin_tests_dsl_common_io.Lin_tests_dsl_common

module IC_domain = Lin_domain.Make(ICConf)
module OC_domain = Lin_domain.Make(OCConf)

let tests =
  IC_domain.neg_lin_test ~count:1000 ~name:"Lin DSL In_channel test with Domain" ::
  if Sys.getenv_opt "OCAML_SYSTEM" = Some "macosx"
  then (
    Printf.printf "Lin DSL Out_channel test with Domain disabled under macOS\n\n%!";
    []
  ) else [
    OC_domain.neg_lin_test ~count:1000 ~name:"Lin DSL Out_channel test with Domain";
  ]

let _ = Util.run_tests_main tests
