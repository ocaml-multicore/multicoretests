open Lin_api

module Sut_int64 =
  struct
    let init () = ref Int64.zero
    let get r = !r
    let set r i = r:=i
    let add r i = let old = !r in r:= Int64.add i old (* buggy: not atomic *)
    let incr r = add r Int64.one                      (* buggy: not atomic *)
    let decr r = add r Int64.minus_one                (* buggy: not atomic *)
end

module Ref_int64_spec : Lin_api.ApiSpec = struct
  type t = int64 ref
  let init () = Sut_int64.init ()
  let cleanup _ = ()
  let int64 = nat64
  let api =
    [ val_ "Sut_int64.get"  Sut_int64.get  (t @-> returning int64);
      val_ "Sut_int64.set"  Sut_int64.set  (t @-> int64 @-> returning unit);
      val_ "Sut_int64.add"  Sut_int64.add  (t @-> int64 @-> returning unit);
      val_ "Sut_int64.incr" Sut_int64.incr (t @-> returning unit);
      val_ "Sut_int64.decr" Sut_int64.decr (t @-> returning unit);
    ]
  end

open QCheck
module RConf_int64 = struct
  type t = int64 ref

  type cmd =
    | Get
    | Set of int'
    | Add of int'
    | Incr
    | Decr [@@deriving qcheck, show { with_path = false }]
  and int' = int64 [@gen Gen.(map Int64.of_int nat)]

  let shrink_cmd c = match c with
    | Get
    | Incr
    | Decr -> Iter.empty
    | Set i -> Iter.map (fun i -> Set i) (Shrink.int64 i)
    | Add i -> Iter.map (fun i -> Add i) (Shrink.int64 i)

  type res = RGet of int64 | RSet | RAdd | RIncr | RDecr [@@deriving show { with_path = false }, eq]

  let init () = Sut_int64.init ()

  let run c r = match c with
    | Get   -> RGet (Sut_int64.get r)
    | Set i -> (Sut_int64.set r i; RSet)
    | Add i -> (Sut_int64.add r i; RAdd)
    | Incr  -> (Sut_int64.incr r; RIncr)
    | Decr  -> (Sut_int64.decr r; RDecr)

  let cleanup _ = ()
end

module RT_int64_DSL = Lin_api.Make(Ref_int64_spec)
module RT_int64_LIN = Lin.Make(RConf_int64)


let buggy cpt prop pg =
  try
    let res = Util.repeat 100 prop pg in
    if not res then incr cpt; true
  with _ -> incr cpt; true

let dsl = ref 0
let lin = ref 0

let buggy_dsl = buggy dsl RT_int64_DSL.lin_prop_thread
let buggy_lin = buggy lin RT_int64_LIN.lin_prop_thread

let count = 10000

let test_dsl = Test.make
                 ~name:"Number of buggy programs with original dsl"
                 ~count
                 (RT_int64_DSL.arb_cmds_par 20 10)
                 buggy_dsl

let test_lin = Test.make
                 ~name:"Number of buggy programs with original lin"
                 ~count
                 (RT_int64_LIN.arb_cmds_par 20 10)
                 buggy_lin

let _ = QCheck_runner.run_tests ~verbose:false [ test_dsl; test_lin ]
let _ = Printf.printf "Buggy programs with original lin: %i / %i\n%!" (!lin) count
let _ = Printf.printf "Buggy programs with dsl:          %i / %i\n%!" (!dsl) count
