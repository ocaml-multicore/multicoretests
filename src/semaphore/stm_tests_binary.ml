open QCheck
open STM

(** parallel STM tests of Semaphore.Binary *)

(* Semaphore API these tests will exercise

  val make : bool -> t
  val release : t -> unit
  val acquire : t -> unit
  val try_acquire : t -> bool

*)

module SBConf =
  struct
    type sut = Semaphore.Binary.t
    type state = int

    type cmd =
      | Release
      | Acquire
      | TryAcquire

    let pp_cmd _ fmt x =
      let open Util.Pp in
      match x with
      | Release -> cst0 "Release" fmt
      | Acquire -> cst0 "Acquire" fmt
      | TryAcquire -> cst0 "TryAcquire" fmt

    let show_cmd = Util.Pp.to_show pp_cmd

    let init_state = 1
    let init_sut () = Semaphore.Binary.make true
    let cleanup _ = ()

    let arb_cmd s =
      let cmds = [ Release; TryAcquire; ] in
      let cmds = if s = 1 then Acquire :: cmds else cmds in
      QCheck.make ~print:show_cmd (Gen.oneofl cmds)

    let next_state c s = match c with
      | Release -> 1
      | Acquire -> 0
      | TryAcquire -> if s = 1 then 0 else 0

    let run c sem =
      match c with
      | Release    -> Res (unit, Semaphore.Binary.release sem)
      | Acquire    -> Res (unit, Semaphore.Binary.acquire sem)
      | TryAcquire -> Res (bool, Semaphore.Binary.try_acquire sem)

    let precond c s =
      match c with
      | Acquire -> s = 1
      | _       -> true
    let postcond c s res =
      match c,res with
      | Release,    Res ((Unit,_), _)
      | Acquire,    Res ((Unit,_), _) -> true
      | TryAcquire, Res ((Bool,_),r)  -> r = (s = 1)
      | _                             -> false
  end

module SBTest_seq = STM_sequential.Make(SBConf)
module SBTest_dom = STM_domain.Make(SBConf)

let _ =
  QCheck_base_runner.run_tests_main
    (let count = 500 in
     [SBTest_seq.agree_test     ~count ~name:"STM Semaphore.Binary test sequential";
      SBTest_dom.agree_test_par ~count ~name:"STM Semaphore.Binary test parallel";
     ])
