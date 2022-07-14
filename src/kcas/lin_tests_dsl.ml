(* ********************************************************************** *)
(*                             Tests of [Kcas]                            *)
(* ********************************************************************** *)
module Common =
struct
  type 'a cas_result
    = 'a Kcas.cas_result =
    | Aborted
    | Failed
    | Success of 'a [@@deriving show { with_path = false }, eq]

  let cas_result ty =
    Lin_api.deconstructible (show_cas_result (fun fmt a -> Format.pp_print_string fmt (Lin_api.print ty a))) (equal_cas_result (Lin_api.equal ty))

  let fun_none _ty =
    let print_fun _ = "(fun _ -> None)" in
    let fun_gen = QCheck.(make ~print:print_fun (Gen.return (fun _ -> None))) in
    Lin_api.gen fun_gen print_fun

  let fun_some _ty =
    let print_fun _ = "(fun i -> Some i)" in
    let fun_gen = QCheck.(make ~print:print_fun (Gen.return (fun i -> Some i))) in
    Lin_api.gen fun_gen print_fun
end

module KConf =
struct
  type t = int Kcas.ref
  let init () = Kcas.ref 0
  let cleanup _ = ()

  open Lin_api
  let int = nat_small
  let fun_none,fun_some,cas_result = Common.(fun_none,fun_some,cas_result)
  let api =
    [ val_ "Kcas.get"     Kcas.get     (t @-> returning int);
      val_ "Kcas.set"     Kcas.set     (t @-> int @-> returning unit);
      val_ "Kcas.cas"     Kcas.cas     (t @-> int @-> int @-> returning bool);
      val_ "Kcas.try_map" Kcas.try_map (t @-> fun_none int @-> returning (cas_result int));
      val_ "Kcas.try_map" Kcas.try_map (t @-> fun_some int @-> returning (cas_result int)); (* Seq,exec cannot fail - hence not linearizable with [try_map] *)
      val_ "Kcas.map"     Kcas.map     (t @-> fun_none int @-> returning (cas_result int));
      val_ "Kcas.map"     Kcas.map     (t @-> fun_some int @-> returning (cas_result int));
      val_ "Kcas.incr"    Kcas.incr    (t @-> returning unit);
      val_ "Kcas.decr"    Kcas.decr    (t @-> returning unit);
    ]
end

module KT = Lin_api.Make(KConf)


(* ********************************************************************** *)
(*                           Tests of [Kcas.W1]                           *)
(* ********************************************************************** *)
module KW1Conf =
struct
  type t = int Kcas.W1.ref
  let init () = Kcas.W1.ref 0
  let cleanup _ = ()

  open Lin_api
  let int = nat_small
  let fun_none,fun_some,cas_result = Common.(fun_none,fun_some,cas_result)
  let api =
    [ val_ "Kcas.W1.get"     Kcas.W1.get     (t @-> returning int);
      val_ "Kcas.W1.set"     Kcas.W1.set     (t @-> int @-> returning unit);
      val_ "Kcas.W1.cas"     Kcas.W1.cas     (t @-> int @-> int @-> returning bool);
      val_ "Kcas.W1.try_map" Kcas.W1.try_map (t @-> fun_none int @-> returning (cas_result int));
      (*val_ "Kcas.W1.try_map" Kcas.W1.try_map (t @-> fun_some int @-> returning (cas_result int));*) (* Seq,exec cannot fail - hence not linearizable with [try_map] *)
      val_ "Kcas.W1.map"     Kcas.W1.map     (t @-> fun_none int @-> returning (cas_result int));
      val_ "Kcas.W1.map"     Kcas.W1.map     (t @-> fun_some int @-> returning (cas_result int));
      val_ "Kcas.W1.incr"    Kcas.W1.incr    (t @-> returning unit);
      val_ "Kcas.W1.decr"    Kcas.W1.decr    (t @-> returning unit);
    ]
end

module KW1T = Lin_api.Make(KW1Conf)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main [
  KT.neg_lin_test `Domain ~count:1000 ~name:"Kcas test";
  KW1T.lin_test   `Domain ~count:1000 ~name:"Kcas.W1 test";
]
