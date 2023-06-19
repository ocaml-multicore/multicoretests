(* Test of the behaviour of STM tests with uncaught exceptions *)

let always_fail () = failwith "unexpected"

module UncaughtExcConf : STM.Spec = struct
  open STM

  type sut = unit
  type state = unit
  type cmd = AlwaysFail of sut

  let show_cmd = function AlwaysFail () -> "AlwaysFail ()"
  let arb_cmd _ = QCheck.(make ~print:show_cmd (Gen.pure (AlwaysFail ())))
  let init_state = ()
  let next_state _ _ = ()
  let init_sut _ = ()
  let cleanup _ = ()
  let precond _ _ = true
  let run c s = match c with AlwaysFail () -> Res (unit, always_fail s)

  let postcond c _ r =
    match (c, r) with AlwaysFail (), Res ((Unit, _), _) -> true | _ -> false
end

module UE = STM_sequential.Make (UncaughtExcConf)

let _ =
  QCheck_base_runner.run_tests_main
    [
      UE.agree_test ~count:10 ~name:"STM test of uncaught exceptions";
      UE.neg_agree_test ~count:10 ~name:"neg STM test of uncaught exceptions";
    ]
