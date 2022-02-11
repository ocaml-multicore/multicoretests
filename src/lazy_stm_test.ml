open QCheck

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
    | Map_val of int_fun [@@deriving show { with_path = false }]
  and int_fun = (int -> int) fun_ [@printer fun fmt f -> fprintf fmt "%s" (Fn.print f)]
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

  type res =
    | RForce of int
    | RForce_val of int
    | RIs_val of bool
    | RMap of int
    | RMap_val of int [@@deriving show { with_path = false }]

  let run c l = match c with
    | Force               -> RForce (Lazy.force l)
    | Force_val           -> RForce_val (Lazy.force_val l)
    | Is_val              -> RIs_val (Lazy.is_val l)
    | Map (Fun (_,f))     -> RMap (Lazy.force (Lazy.map f l)) (*we force the "new lazy"*)
    | Map_val (Fun (_,f)) -> RMap_val (Lazy.force (Lazy.map_val f l)) (*we force the "new lazy"*)

  let postcond c s res = match c,res with
    | Force,               RForce v
    | Force_val,           RForce_val v -> v = fst s
    | Is_val,              RIs_val r    -> r = snd s
    | Map (Fun (_,f)),     RMap i
    | Map_val (Fun (_,f)), RMap_val i   -> i = f (fst s)
    | _,_ -> false
end


module LTlazy    = STM.Make(struct
    include LConfbase
    let init_state  = (7 * 100, false)
    let init_sut () = lazy (work ())
  end)
module LTfromval = STM.Make(struct
    include LConfbase
    let init_state = (42, true)
    let init_sut () = Lazy.from_val 42
  end)
module LTfromfun = STM.Make(struct
    include LConfbase
    let init_state = (7 * 100, false)
    let init_sut () = Lazy.from_fun work
  end)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count = 100 in
   [LTlazy.agree_test        ~count ~name:"lazy test";
    LTfromval.agree_test     ~count ~name:"lazy test from_val";
    LTfromfun.agree_test     ~count ~name:"lazy test from_fun";
    LTlazy.agree_test_par    ~count ~name:"lazy test";
    LTfromval.agree_test_par ~count ~name:"lazy test from_val";
    LTfromfun.agree_test_par ~count ~name:"lazy test from_fun";
   ])
