(* Test of the behaviour of Lin tests with uncaught exceptions *)

let always_fail () = failwith "unexpected"

module UncaughtExcConf = struct
  open Lin

  type t = unit

  let init () = ()
  let cleanup _ = ()
  let api = [ val_ "always_fail" always_fail (t @-> returning unit) ]
end

module UncaughtExc_domain = Lin_domain.Make (UncaughtExcConf)

let _ =
  QCheck_base_runner.run_tests_main
    [
      UncaughtExc_domain.lin_test ~count:10
        ~name:"Lin DSL test of uncaught exceptions";
      UncaughtExc_domain.neg_lin_test ~count:10
        ~name:"neg Lin DSL test of uncaught exceptions";
    ]
