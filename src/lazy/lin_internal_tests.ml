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
  type cmd =
    | Force
    | Force_val
    | Is_val
    | Map of int_fun
    | Map_val of int_fun
  and int_fun = (int -> int) fun_

  let pp_cmd par fmt x =
    let open Util.Pp in
    let pp_int_fun = of_show Fn.print in
    match x with
    | Force -> cst0 "Force" fmt
    | Force_val -> cst0 "Force_val" fmt
    | Is_val -> cst0 "Is_val" fmt
    | Map x -> cst1 pp_int_fun "Map" par fmt x
    | Map_val x -> cst1 pp_int_fun "Map_val" par fmt x

  let show_cmd = Util.Pp.to_show pp_cmd

  let gen_cmd =
    let open QCheck.Gen in
    let int_fun = (fun1 Observable.int QCheck.small_nat).gen in
    oneof
      [
        pure Force;
        pure Force_val;
        pure Is_val;
        map (fun x -> Map x) int_fun;
        map (fun x -> Map_val x) int_fun;
      ]

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

  type res =
    | RForce of (int, exn) result
    | RForce_val of (int, exn) result
    | RIs_val of bool
    | RMap of (int, exn) result
    | RMap_val of (int, exn) result

  let pp_res par fmt x =
    let open Util.Pp in
    match x with
    | RForce x -> cst1 (pp_result pp_int pp_exn) "RForce" par fmt x
    | RForce_val x -> cst1 (pp_result pp_int pp_exn) "RForce_val" par fmt x
    | RIs_val x -> cst1 pp_bool "RIs_val" par fmt x
    | RMap x -> cst1 (pp_result pp_int pp_exn) "RMap" par fmt x
    | RMap_val x -> cst1 (pp_result pp_int pp_exn) "RMap_val" par fmt x

  let show_res = Util.Pp.to_show pp_res

  let equal_res x y =
    let open Util.Equal in
    match (x, y) with
    | RForce x, RForce y -> equal_result equal_int equal_exn x y
    | RForce_val x, RForce_val y -> equal_result equal_int equal_exn x y
    | RIs_val x, RIs_val y -> equal_bool x y
    | RMap x, RMap y -> equal_result equal_int equal_exn x y
    | RMap_val x, RMap_val y -> equal_result equal_int equal_exn x y
    | _, _ -> false

  let run c l = match c with
    | Force               -> RForce (Util.protect Lazy.force l)
    | Force_val           -> RForce_val (Util.protect Lazy.force_val l)
    | Is_val              -> RIs_val (Lazy.is_val l)
    | Map (Fun (_,f))     -> RMap (try Ok (Lazy.force (Lazy.map f l))
                                   with exn -> Error exn) (*we force the "new lazy"*)
    | Map_val (Fun (_,f)) -> RMap_val (try Ok (Lazy.force (Lazy.map_val f l))
                                       with exn -> Error exn) (*we force the "new lazy"*)
end


module LTlazy    = Lin_domain.Make_internal(struct
    include LBase
    let init () = lazy (work ())
  end) [@alert "-internal"]
module LTfromval = Lin_domain.Make_internal(struct
    include LBase
    let init () = Lazy.from_val 42
  end) [@alert "-internal"]
module LTfromfun = Lin_domain.Make_internal(struct
    include LBase
    let init () = Lazy.from_fun work
  end) [@alert "-internal"]
;;
QCheck_base_runner.run_tests_main
  (let count = 100 in
   [LTlazy.neg_lin_test       ~count ~name:"Lin.Internal Lazy test with Domain";
    LTfromval.lin_test        ~count ~name:"Lin.Internal Lazy test with Domain from_val";
    LTfromfun.neg_lin_test    ~count ~name:"Lin.Internal Lazy test with Domain from_fun";
   ])
