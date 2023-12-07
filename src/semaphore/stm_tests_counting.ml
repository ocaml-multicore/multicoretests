open QCheck
open STM

(** parallel STM tests of Semaphore.Counting *)

(* Semaphore API these tests will exercise

  val make : int -> t
  val release : t -> unit
  val acquire : t -> unit
  val try_acquire : t -> bool
  val get_value : t -> int

*)

module SC = Semaphore.Counting

module SCConf =
  struct
    type sut = SC.t
    type state = int

    type cmd =
      | Release
      | Acquire
      | TryAcquire
      | GetValue

    let pp_cmd _ fmt x =
      let open Util.Pp in
      match x with
      | Release -> cst0 "Release" fmt
      | Acquire -> cst0 "Acquire" fmt
      | TryAcquire -> cst0 "TryAcquire" fmt
      | GetValue -> cst0 "GetValue" fmt

    let show_cmd = Util.Pp.to_show pp_cmd

    let init_state = 2

    let init_sut () = SC.make init_state
    let cleanup _ = ()

    let arb_cmd s =
      let cmds = [ Release; TryAcquire; GetValue ] in
      let cmds = if s > 0 then Acquire :: cmds else cmds in
      QCheck.make ~print:show_cmd (Gen.oneofl cmds)

    let next_state c s = match c with
      | Release -> s+1
      | Acquire -> s-1
      | TryAcquire -> if s > 0 then s-1 else s
      | GetValue -> s

    let run c sem =
      match c with
      | Release    -> Res (unit, SC.release sem)
      | Acquire    -> Res (unit, SC.acquire sem)
      | TryAcquire -> Res (bool, SC.try_acquire sem)
      | GetValue   -> Res (int,  SC.get_value sem)

    let precond c s =
      match c with
      | Acquire -> s > 0
      | _       -> true
    let postcond c s res =
      match c,res with
      | Release,    Res ((Unit,_), _)
      | Acquire,    Res ((Unit,_), _) -> true
      | TryAcquire, Res ((Bool,_),r)  -> r = (s > 0)
      | GetValue,   Res ((Int,_),r)   -> r = s
      | _                             -> false
  end

module SCTest_seq = STM_sequential.Make(SCConf)
module SCTest_dom_gv = STM_domain.Make(SCConf)
module SCTest_dom = STM_domain.Make(struct
    include SCConf
    let arb_cmd s =
      let cmds = [ Release; TryAcquire; ] in (* No GetValue *)
      let cmds = if s > 0 then Acquire :: cmds else cmds in
      QCheck.make ~print:show_cmd (Gen.oneofl cmds)
  end)

let _ =
  QCheck_base_runner.run_tests_main
    (let count = 200 in
     [SCTest_seq.agree_test        ~count ~name:"STM Semaphore.Counting test sequential";
      SCTest_dom_gv.agree_test_par ~count ~name:"STM Semaphore.Counting test parallel (w/ get_value)";
      SCTest_dom.agree_test_par    ~count ~name:"STM Semaphore.Counting test parallel (w/o get_value)";
     ])
