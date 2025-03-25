open Stm_tests_spec_ref

module RT_int   = STM_thread.Make(RConf_int)
module RT_int64 = STM_thread.Make(RConf_int64)
;;
if Sys.(ocaml_release.major,ocaml_release.minor) < (5,3)
then Printf.printf "STM.thread ref tests disabled on OCaml 5.2 and earlier\n%!"
else
  QCheck_base_runner.run_tests_main
    [RT_int.agree_test_conc       ~count:250  ~name:"STM int ref test with Thread";
     if Sys.backend_type = Sys.Bytecode (* test is considered positive under bytecode *)
     then
       RT_int64.agree_test_conc     ~count:1000 ~name:"STM int64 ref test with Thread"
     else
       RT_int64.neg_agree_test_conc ~count:5000 ~name:"STM int64 ref test with Thread";
    ]
