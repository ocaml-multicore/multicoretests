(* ************************************ *)
(*           Tests of Dynlink           *)
(* ************************************ *)

open Lin

(* Two libraries that should exist, one that should not *)
let library_name = QCheck.Gen.oneofl ["libA.cma"; "libB.cma"; "libC.cma"]
let arb_library = QCheck.make library_name
let print_library l = QCheck.Print.string (Dynlink.adapt_filename l)

(** A {!Lin} {i type} for files that can be dynamically linked *)
let library = gen_deconstructible arb_library print_library (=)

let loadfile f = Dynlink.loadfile (Dynlink.adapt_filename f)

module DynConf =
struct
  type t = unit

  let init () = ()
  let cleanup _ = ()

  let api =
    [ val_ "Dynlink.loadfile"           loadfile                   (library @-> returning_or_exc unit);
      val_ "Dynlink.main_program_units" Dynlink.main_program_units (unit @-> returning (list string));
      val_ "Dynlink.all_units"          Dynlink.all_units          (unit @-> returning (list string));
    ]
end

module DynT = Lin_domain.Make(DynConf)

let _ =
  QCheck_base_runner.run_tests_main [
    DynT.neg_lin_test ~count:100 ~name:"negative Lin DSL Dynlink test with Domain";
  ]
