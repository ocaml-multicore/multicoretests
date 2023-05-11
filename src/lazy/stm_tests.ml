open QCheck
open STM

(** parallel STM tests of Lazy *)

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

module LConfbase =
struct
  type cmd =
    | Force
    | Force_val
    | Is_val
    | Map of int_fun
    | Map_val of int_fun
  and int_fun = (int -> int) fun_

  let pp_int_fun par fmt f =
    Format.fprintf fmt (if par then "(%s)" else "%s") (Fn.print f)

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Force -> cst0 "Force" fmt
    | Force_val -> cst0 "Force_val" fmt
    | Is_val -> cst0 "Is_val" fmt
    | Map x -> cst1 pp_int_fun "Map" par fmt x
    | Map_val x -> cst1 pp_int_fun "Map_val" par fmt x

  let show_cmd = Util.Pp.to_show pp_cmd

  type state = int * bool
  type sut = int Lazy.t

  let arb_cmd _s =
    let int' = small_nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [Gen.return Force;
          Gen.return Force_val;
          Gen.return Is_val;
          Gen.map (fun f -> Map f)     (fun1 Observable.int int').gen;
          Gen.map (fun f -> Map_val f) (fun1 Observable.int int').gen;
         ])

  let cleanup _   = ()

  let next_state c s = match c with
    | Force
    | Force_val -> (fst s,true)
    | Is_val    -> s
    | Map _
    | Map_val _ -> (fst s,true) (*run forces the "lazy child"*)

  let precond _ _ = true

  (*
  type res =
    | RForce of (int,exn) result
    | RForce_val of (int,exn) result
    | RIs_val of bool
    | RMap of (int,exn) result
    | RMap_val of (int,exn) result [@@deriving show { with_path = false }]
  *)

  let run c l =
    match c with
    | Force               -> Res (result int exn, protect Lazy.force l)
    | Force_val           -> Res (result int exn, protect Lazy.force_val l)
    | Is_val              -> Res (bool, Lazy.is_val l)
    | Map (Fun (_,f))     ->
        Res (result int exn, try Ok (Lazy.force (Lazy.map f l))
                             with exn -> Error exn) (*we force the "new lazy"*)
    | Map_val (Fun (_,f)) ->
        Res (result int exn, try Ok (Lazy.force (Lazy.map_val f l))
                             with exn -> Error exn) (*we force the "new lazy"*)

  let postcond c (s : state) res =
    match c,res with
    | (Force|Force_val),
      Res ((Result (Int,Exn), _), v) -> v = Ok (fst s)
    | Is_val,       Res ((Bool,_),r) -> r = snd s
    | (Map (Fun (_,f)) | Map_val (Fun (_,f))),
      Res ((Result (Int,Exn), _), i) -> i = Ok (f (fst s))
    | _,_ -> false
end

module LTlazy_seq    = STM_sequential.Make(struct
    include LConfbase
    let init_state  = (7 * 100, false)
    let init_sut () = lazy (work ())
  end)
module LTfromval_seq = STM_sequential.Make(struct
    include LConfbase
    let init_state = (42, true)
    let init_sut () = Lazy.from_val 42
  end)
module LTfromfun_seq = STM_sequential.Make(struct
    include LConfbase
    let init_state = (7 * 100, false)
    let init_sut () = Lazy.from_fun work
  end)

module LTlazy_dom    = STM_domain.Make(struct
    include LConfbase
    let init_state  = (7 * 100, false)
    let init_sut () = lazy (work ())
  end)
module LTfromval_dom = STM_domain.Make(struct
    include LConfbase
    let init_state = (42, true)
    let init_sut () = Lazy.from_val 42
  end)
module LTfromfun_dom = STM_domain.Make(struct
    include LConfbase
    let init_state = (7 * 100, false)
    let init_sut () = Lazy.from_fun work
  end)
;;
QCheck_base_runner.run_tests_main
  (let count = 200 in
   [LTlazy_seq.agree_test        ~count ~name:"STM Lazy test sequential";
    LTfromval_seq.agree_test     ~count ~name:"STM Lazy test sequential from_val";
    LTfromfun_seq.agree_test     ~count ~name:"STM Lazy test sequential from_fun";
    LTlazy_dom.neg_agree_test_par    ~count ~name:"STM Lazy test parallel";
    LTfromval_dom.agree_test_par     ~count ~name:"STM Lazy test parallel from_val";
    LTfromfun_dom.neg_agree_test_par ~count ~name:"STM Lazy test parallel from_fun";
   ])
