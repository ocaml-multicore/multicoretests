open QCheck

let ref_count = (*1000*) 10000
let clist_count = (*250*) 10000

type stat_test =
    Stat_test : {
      name                : string;
      prop                : 'a -> bool;
      cmds_gen            : int -> int -> 'a QCheck.arbitrary;
      count               : int;
      mutable issue_count : int;
    } -> stat_test

let make_stat_test (Stat_test stat_test) =
  let adjusted_prop triple =
    try
      let res = Util.repeat 100 stat_test.prop triple in
      if not res then stat_test.issue_count <- 1 + stat_test.issue_count; true
    with _ -> stat_test.issue_count <- 1 + stat_test.issue_count; true
  in
  Test.make ~count:stat_test.count ~name:stat_test.name
    (stat_test.cmds_gen 20 12) adjusted_prop

let test_desc_list = [
  [ (* ********************** Domain and Lin_api/Lin ********************** *)
    Stat_test {
      name        = "Buggy ref int64 programs with Domain + Lin_api";
      prop        = Lin_tests_common_dsl.RT_int64.lin_prop_domain;
      cmds_gen    = Lin_tests_common_dsl.RT_int64.arb_cmds_par;
      count       = ref_count;
      issue_count = 0;
    };
    Stat_test {
      name        = "Buggy ref int64 programs with Domain + plain Lin";
      prop        = Lin_tests_common.RT_int64.lin_prop_domain;
      cmds_gen    = Lin_tests_common.RT_int64.arb_cmds_par;
      count       = ref_count;
      issue_count = 0;
    };
  ];
  [ (* ********************** Thread and Lin_api ********************** *)
    Stat_test {
      name        = "Buggy ref int programs with Thread + Lin_api";
      prop        = Lin_tests_common_dsl.RT_int.lin_prop_thread;
      cmds_gen    = Lin_tests_common_dsl.RT_int.arb_cmds_par;
      count       = ref_count;
      issue_count = 0;
    };
    Stat_test {
      name        = "Buggy ref int64 programs with Thread + Lin_api";
      prop        = Lin_tests_common_dsl.RT_int64.lin_prop_thread;
      cmds_gen    = Lin_tests_common_dsl.RT_int64.arb_cmds_par;
      count       = ref_count;
      issue_count = 0;
    };
    Stat_test {
      name        = "Buggy CList int programs with Thread + Lin_api";
      prop        = Lin_tests_common_dsl.CLT_int.lin_prop_thread;
      cmds_gen    = Lin_tests_common_dsl.CLT_int.arb_cmds_par;
      count       = clist_count;
      issue_count = 0;
    };
    Stat_test {
      name        = "Buggy CList int64 programs with Thread + Lin_api";
      prop        = Lin_tests_common_dsl.CLT_int64.lin_prop_thread;
      cmds_gen    = Lin_tests_common_dsl.CLT_int64.arb_cmds_par;
      count       = clist_count;
      issue_count = 0;
    };
  ];
  [ (* ********************** Thread and plain Lin ********************** *)
    Stat_test {
      name        = "Buggy ref int programs with Thread + plain Lin";
      prop        = Lin_tests_common.RT_int.lin_prop_thread;
      cmds_gen    = Lin_tests_common.RT_int.arb_cmds_par;
      count       = ref_count;
      issue_count = 0;
    };
    Stat_test {
      name        = "Buggy ref int64 programs with Thread + plain Lin";
      prop        = Lin_tests_common.RT_int64.lin_prop_thread;
      cmds_gen    = Lin_tests_common.RT_int64.arb_cmds_par;
      count       = ref_count;
      issue_count = 0;
    };
    Stat_test {
      name        = "Buggy CList int programs with Thread + plain Lin";
      prop        = Lin_tests_common.CLT_int.lin_prop_thread;
      cmds_gen    = Lin_tests_common.CLT_int.arb_cmds_par;
      count       = clist_count;
      issue_count = 0;
    };
    Stat_test {
      name        = "Buggy CList int64 programs with Thread + plain Lin";
      prop        = Lin_tests_common.CLT_int64.lin_prop_thread;
      cmds_gen    = Lin_tests_common.CLT_int64.arb_cmds_par;
      count       = clist_count;
      issue_count = 0;
    };
  ];
]

let print_summary () =
  Printf.printf "\nStats:\n\n";
  List.iter (fun sub_list ->
      List.iter (fun (Stat_test t) ->
          Printf.printf "%-50s %4i / %4i\n%!" t.name t.issue_count t.count)
        sub_list;
      Printf.printf "\n%!")
    test_desc_list

let _ = QCheck_runner.run_tests ~verbose:true (List.map make_stat_test (List.concat test_desc_list))
let () = print_summary ()
