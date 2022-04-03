open Lin_tests_common

(** This is a driver of the negative tests over the Domain module *)

(*
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count = 1000 in
   [RT_int.lin_test    `Domain ~count ~name:"ref int test";
    RT_int64.lin_test  `Domain ~count ~name:"ref int64 test";
    CLT_int.lin_test   `Domain ~count ~name:"CList int test";
    CLT_int64.lin_test `Domain ~count ~name:"CList int64 test"])
*)

;;
(* Avoid printing twice:
   - from QCheck2.Test.check_cell_exn and
   - from installed exception printer *)
Printexc.register_printer (fun exn -> match exn with
    | QCheck2.Test_exceptions.Test_fail _ -> Some (Printexc.to_string_default exn)
    | _ -> None)

let negate (name,flag,run) : unit Alcotest.test_case = (name,flag,fun () ->
  try
    begin
      run ();
      failwith (Printf.sprintf "Test %s succeeded but was expected to fail" name)
    end
  with (QCheck2.Test_exceptions.Test_fail _) as e ->
    begin
      Printf.printf "Test %s failed as expected with exception %s\n%!" name (Printexc.to_string_default e);
      ()
    end)

let to_positive_alcotest = QCheck_alcotest.to_alcotest
let to_negative_alcotest t = negate (QCheck_alcotest.to_alcotest t)

let suite_negative =
  List.map to_negative_alcotest
    (let count = 1000 in
     [RT_int.lin_test    `Domain ~count ~name:"ref int test";
      RT_int64.lin_test  `Domain ~count ~name:"ref int64 test";
      CLT_int.lin_test   `Domain ~count ~name:"CList int test";
      CLT_int64.lin_test `Domain ~count ~name:"CList int64 test";
      QCheck.(Test.make ~name:"a-negative-test-that-succeeds" ~count:100 small_int (fun i -> i = i));
     ])

let suite_positive =
  List.map to_positive_alcotest
    (let count = 1000 in
     [QCheck.(Test.make ~name:"positive test" ~count:100 small_int (fun i -> i=i));
      RT_int.lin_test    `Domain ~count ~name:"ref int test";
     ])

let () =
  (*Printexc.record_backtrace true;*)
  Alcotest.run ~verbose:true (*~compact:true*) ~show_errors:true ~record_backtrace:false "neg_tests" [
    "domain_lin_tests negative", suite_negative;
    "domain_lin_tests positive", suite_positive;
  ];

(*

Alcotest offers
 - a compact/non-compact switch
 - a less noisy verbose printer
 - continuous output as tests are run
 - marking tests as positive/negative with the above

Assuming one wants
 - negative tests with shrunk output printed, the above output is a bit hard to tell apart:
   ```
     ...     [test family]        0   Test name
   test `Test name` failed on ≥ 1 cases:
   [...]
     [OK]    [test family]        0   Test name
   ```

Seeing the found counterexamples in the CI-log can be useful to tell if shrinkers are working as expected.


Alcotest is missing
 - timings on individual tests (nice to tell what is taking the time)

*)
