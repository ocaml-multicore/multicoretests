open Stm_tests_clist_spec
module CList_bis = struct include CList let add_node = add_node_thread end (* moves an allocation to trigger thread unsafety *)
module CLT_int_thread = STM_thread.Make(CLConf(CList_bis)(Int))
module CLT_int64_thread = STM_thread.Make(CLConf(CList_bis)(Int64))
;;
if Sys.(ocaml_release.major,ocaml_release.minor) < (5,3)
then Printf.printf "STM.thread CList tests disabled on OCaml 5.2 and earlier\n%!"
else
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [CLT_int_thread.neg_agree_test_conc   ~count ~name:"STM int CList test with Thread";
      CLT_int64_thread.neg_agree_test_conc ~count ~name:"STM int64 CList test with Thread"])
