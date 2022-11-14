module Atomic_spec : Lin_api.ApiSpec = struct
  open Lin_api (* FIXME add Gen.nat *)
  type t = int Atomic.t
  let init () = Atomic.make 0
  let cleanup _ = ()
  let api =
    [ val_ "Atomic.get"             Atomic.get             (t @-> returning int);
      val_ "Atomic.set"             Atomic.set             (t @-> int @-> returning unit);
      val_ "Atomic.exchange"        Atomic.exchange        (t @-> int @-> returning int);
      val_ "Atomic.fetch_and_add"   Atomic.fetch_and_add   (t @-> int @-> returning int);
      val_ "Atomic.compare_and_set" Atomic.compare_and_set (t @-> int @-> int @-> returning bool);
      val_ "Atomic.incr"            Atomic.incr            (t @-> returning unit);
      val_ "Atomic.decr"            Atomic.decr            (t @-> returning unit) ]
end

module Lin_atomic = Lin_api.Make (Atomic_spec)

let () =
  QCheck_base_runner.run_tests_main
    [ Lin_atomic.lin_test `Domain ~count:1000 ~name:"Lin_api Atomic test with Domain";
    ]
