open QCheck

(** ********************************************************************** *)
(**                      Tests of thread-unsafe [Array]                    *)
(** ********************************************************************** *)
module AConf =
struct
  type t = char array

 (* note to self:
    - generate one t_var per input [t] parameter and
    - record [Env.next] for each resulting [t] in an option *)
  open Lin
  type cmd =
    | Length of Var.t
    | Get of Var.t * int
    | Set of Var.t * int * char
    | Append of Var.t * Var.t
    | Sub of Var.t * int * int
    | Copy of Var.t
    | Fill of Var.t * int * int * char
    | To_list of Var.t
    | Mem of Var.t * char
    | Sort of Var.t
    | To_seq of Var.t [@@deriving show { with_path = false }]

  let gen_int = Gen.small_nat
  let gen_char = Gen.printable
  let map4 f w x y z st = f (w st) (x st) (y st) (z st)
  let gen_cmd gen_var =
    Gen.(oneof [
        map  (fun t -> (None, Length t)) gen_var;
        map2 (fun t i -> (None, Get (t,i))) gen_var gen_int;
        map3 (fun t i c -> (None, Set (t,i,c))) gen_var gen_int gen_char;
        map2 (fun t1 t2 -> (Some (Var.next ()), Append (t1,t2))) gen_var gen_var;
        map3 (fun v i l -> (Some (Var.next ()), Sub (v,i,l))) gen_var gen_int gen_int;
        map  (fun t -> (Some (Var.next ()), Copy t)) gen_var;
        map4 (fun v i l c -> (None, Fill (v,i,l,c))) gen_var gen_int gen_int gen_char;
        map  (fun v -> (None, To_list v)) gen_var;
        map2 (fun v c -> (None, Mem (v,c))) gen_var gen_char;
        map  (fun v -> (None, Sort v)) gen_var;
        map  (fun v -> (None, To_seq v)) gen_var;
      ])

  let shrink_cmd c = match c with
    | Length v     -> Iter.map (fun v -> Length v)    (Var.shrink v)
    | Get (v,i)    -> Iter.map (fun v -> Get (v,i))   (Var.shrink v)
    | Set (v,i,c)  -> Iter.map (fun v -> Set (v,i,c)) (Var.shrink v)
    | Append (v,w) ->
      Iter.(map (fun v -> Append (v,w)) (Var.shrink v)
            <+>
            map (fun w -> Append (v,w)) (Var.shrink w))
    | Sub (v,i,l) -> Iter.map (fun v -> Sub (v,i,l)) (Var.shrink v)
    | Copy v      -> Iter.map (fun v -> Copy v)      (Var.shrink v)
    | Fill (v,i,l,c) -> Iter.map (fun v -> Fill (v,i,l,c)) (Var.shrink v)
    | To_list v   -> Iter.map (fun v -> To_list v)   (Var.shrink v)
    | Mem (v,c)   -> Iter.map (fun v -> Mem (v,c))   (Var.shrink v)
    | Sort v      -> Iter.map (fun v -> Sort v)      (Var.shrink v)
    | To_seq v    -> Iter.map (fun v -> To_seq v)    (Var.shrink v)

  let fix_cmd env = function
    | Length i       -> Iter.map (fun i -> Length i      ) (Env.valid_t_vars env i)
    | Get (i,x)      -> Iter.map (fun i -> Get (i,x)     ) (Env.valid_t_vars env i)
    | Set (i,x,z)    -> Iter.map (fun i -> Set (i,x,z)   ) (Env.valid_t_vars env i)
    | Sub (i,x,y)    -> Iter.map (fun i -> Sub (i,x,y)   ) (Env.valid_t_vars env i)
    | Copy i         -> Iter.map (fun i -> Copy i        ) (Env.valid_t_vars env i)
    | Fill (i,x,y,z) -> Iter.map (fun i -> Fill (i,x,y,z)) (Env.valid_t_vars env i)
    | To_list i      -> Iter.map (fun i -> To_list i     ) (Env.valid_t_vars env i)
    | Mem (i,z)      -> Iter.map (fun i -> Mem (i,z)     ) (Env.valid_t_vars env i)
    | Sort i         -> Iter.map (fun i -> Sort i        ) (Env.valid_t_vars env i)
    | To_seq i       -> Iter.map (fun i -> To_seq i      ) (Env.valid_t_vars env i)
    | Append (i,j)   -> Iter.(map (fun i j -> Append (i,j)) (Env.valid_t_vars env i) <*> (Env.valid_t_vars env j))

  open Util
  (*let pp_exn = Util.pp_exn*)
  let pp fmt t = Format.fprintf fmt "%s" (QCheck.Print.(array char) t)
  let equal a1 a2 = Array.for_all2 (=) a1 a2
  type res =
    | RLength of int
    | RGet of ((char, exn) result)
    | RSet of ((unit, exn) result)
    | RAppend of unit
    | RSub of ((unit, exn) result)
    | RCopy of unit
    | RFill of ((unit, exn) result)
    | RTo_list of char list
    | RMem of bool
    | RSort of unit
    | RTo_seq of (char Seq.t [@printer fun fmt seq -> fprintf fmt "%s" (QCheck.Print.(list char) (List.of_seq seq))])
  [@@deriving show { with_path = false }, eq]

  let array_size = 2

  let init () = Array.make array_size 'a'

  (* FIXME:
     - shrink target variables generically:
       match on optional target i, unless someone is refering to it (how to tell for arb. cmds?)
  *)
  let run c a = match c with
    | None,   Length v       -> RLength (Array.length a.(v))
    | None,   Get (v,i)      -> RGet (Util.protect (Array.get a.(v)) i)
    | None,   Set (v,i,c)    -> RSet (Util.protect (Array.set a.(v) i) c)
    | Some r, Append (v,w)   -> RAppend (let tmp = Array.append a.(v) a.(w) in a.(r) <- tmp)
    | Some r, Sub (v,i,l)    -> RSub (Util.protect (fun () -> let tmp = Array.sub a.(v) i l in a.(r) <- tmp) ())
    | Some r, Copy v         -> RCopy (let tmp = Array.copy a.(v) in a.(r) <- tmp)
    | None,   Fill (v,i,l,c) -> RFill (Util.protect (Array.fill a.(v) i l) c)
    | None,   To_list v      -> RTo_list (Array.to_list a.(v))
    | None,   Mem (v,c)      -> RMem (Array.mem c a.(v))
    | None,   Sort v         -> RSort (Array.sort Char.compare a.(v))
    | None,   To_seq v       -> RTo_seq (List.to_seq (List.of_seq (Array.to_seq a.(v)))) (* workaround: Array.to_seq is lazy and will otherwise see and report later Array.set state changes... *)
    | _, _ -> failwith (Printf.sprintf "unexpected command: %s" (show_cmd (snd c)))
  let cleanup _ = ()
end

module AT = Lin.Make(AConf)
;;
Util.set_ci_printing ()
;;
QCheck_base_runner.run_tests_main [
  AT.neg_lin_test `Domain ~count:1000 ~name:"Lin Array test with Domain";
]
