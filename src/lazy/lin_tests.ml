open QCheck

(** parallel linearization tests of Lazy *)

(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
           else z

let work () =
  let r = ref 0 in
  for _ = 1 to 100 do
    r := !r + tak 18 12 6;
    (*assert (7 = tak 18 12 6);*)
  done;
  !r

(*
module Lazy :
  sig
    type 'a t = 'a CamlinternalLazy.t
    exception Undefined
    external force : 'a t -> 'a = "%lazy_force"
    val map : ('a -> 'b) -> 'a t -> 'b t
    val is_val : 'a t -> bool
    val from_val : 'a -> 'a t
    val map_val : ('a -> 'b) -> 'a t -> 'b t
    val from_fun : (unit -> 'a) -> 'a t
    val force_val : 'a t -> 'a
  end
*)

module LBase =
struct
  open Lin
  type cmd =
    | Force of Var.t
    | Force_val of Var.t
    | Is_val of Var.t
    | Map of Var.t * int_fun
    | Map_val of Var.t * int_fun [@@deriving show { with_path = false }]
  and int_fun = (int -> int) fun_ [@printer fun fmt f -> fprintf fmt "%s" (Fn.print f)]

  let int_fun_gen = (fun1 Observable.int small_nat).gen

  let gen_cmd gen_var =
    Gen.(oneof [
        map  (fun t -> None,Force t) gen_var;
        map  (fun t -> None,Force_val t) gen_var;
        map  (fun t -> None,Is_val t) gen_var;
        map2 (fun t f -> None,Map (t,f)) gen_var int_fun_gen;
        map2 (fun t f -> None,Map_val (t,f)) gen_var int_fun_gen;
      ])
  (*
  let shrink_cmd c = match c with
    | Force
    | Force_val
    | Is_val -> Iter.empty
    | Map f -> Iter.map (fun f -> Map f) (Fn.shrink f)
    | Map_val f -> Iter.map (fun f -> Map_val f) (Fn.shrink f)
  *)
  (* the Lazy tests already take a while to run - so better avoid spending extra time shrinking *)
  let shrink_cmd = Shrink.nil

  type t = int Lazy.t

  let cleanup _ = ()

  let equal_exn = (=)
  type res =
    | RForce of (int,exn) result
    | RForce_val of (int,exn) result
    | RIs_val of bool
    | RMap of (int,exn) result
    | RMap_val of (int,exn) result [@@deriving show { with_path = false }, eq]

  let run c l = match c with
    | None,Force t               -> RForce (Util.protect Lazy.force l.(t))
    | None,Force_val t           -> RForce_val (Util.protect Lazy.force_val l.(t))
    | None,Is_val t              -> RIs_val (Lazy.is_val l.(t))
    | None,Map (t,Fun (_,f))     -> RMap (try Ok (Lazy.force (Lazy.map f l.(t)))
                                          with exn -> Error exn) (*we force the "new lazy"*)
    | None,Map_val (t,Fun (_,f)) -> RMap_val (try Ok (Lazy.force (Lazy.map_val f l.(t)))
                                              with exn -> Error exn) (*we force the "new lazy"*)
    | _, _ -> failwith (Printf.sprintf "unexpected command: %s" (show_cmd (snd c)))
end


module LTlazy    = Lin.Make(struct
    include LBase
    let init () = lazy (work ())
  end)
module LTfromval = Lin.Make(struct
    include LBase
    let init () = Lazy.from_val 42
  end)
module LTfromfun = Lin.Make(struct
    include LBase
    let init () = Lazy.from_fun work
  end)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count = 100 in
   [LTlazy.neg_lin_test       `Domain ~count ~name:"Lin Lazy test with Domain";
    LTfromval.lin_test        `Domain ~count ~name:"Lin Lazy test with Domain from_val";
    LTfromfun.neg_lin_test    `Domain ~count ~name:"Lin Lazy test with Domain from_fun";
   ])
