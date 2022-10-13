open QCheck
open Lin

(** ********************************************************************** *)
(**                      Tests of thread-unsafe [Hashtbl]                  *)
(** ********************************************************************** *)
module HConf =
struct
  type t = (char, int) Hashtbl.t

  type cmd =
    | Clear of Var.t
    | Copy of Var.t
    | Add of Var.t * char * int
    | Remove of Var.t * char
    | Find of Var.t * char
    | Find_opt of Var.t * char
    | Find_all of Var.t * char
    | Replace of Var.t * char * int
    | Mem of Var.t * char
    | Length of Var.t [@@deriving show { with_path = false }]
  let gen_int = Gen.nat
  let gen_char = Gen.printable

  let gen_cmd gen_var =
    Gen.(oneof [
        map  (fun v -> None,Clear v) gen_var;
        map  (fun v -> Some (Var.next ()),Copy v) gen_var;
        map3 (fun v c i -> None,Add (v,c,i)) gen_var gen_char gen_int;
        map2 (fun v c -> None,Remove (v,c)) gen_var gen_char;
        map2 (fun v c -> None,Find (v,c)) gen_var gen_char;
        map2 (fun v c -> None,Find_opt (v,c)) gen_var gen_char;
        map2 (fun v c -> None,Find_all (v,c)) gen_var gen_char;
        map3 (fun v c i -> None,Replace (v,c,i)) gen_var gen_char gen_int;
        map2 (fun v c -> None,Mem (v,c)) gen_var gen_char;
        map  (fun v -> None,Length v) gen_var;
      ])
  let shrink_cmd c = match c with
    | Clear _ -> Iter.empty
    | Copy _ -> Iter.empty
    | Add (v,c,i) ->
        Iter.((map (fun c -> Add (v,c,i)) (Shrink.char c))
              <+>
              (map (fun i -> Add (v,c,i)) (Shrink.int i)))
    | Remove (v,c) -> Iter.map (fun c -> Remove (v,c)) (Shrink.char c)
    | Find (v,c) -> Iter.map (fun c -> Find (v,c)) (Shrink.char c)
    | Find_opt (v,c) -> Iter.map (fun c -> Find_opt (v,c)) (Shrink.char c)
    | Find_all (v,c) -> Iter.map (fun c -> Find_all (v,c)) (Shrink.char c)
    | Replace (v,c,i) ->
        Iter.((map (fun c -> Replace (v,c,i)) (Shrink.char c))
              <+>
              (map (fun i -> Replace (v,c,i)) (Shrink.int i)))
    | Mem (v,c) -> Iter.map (fun c -> Mem (v,c)) (Shrink.char c)
    | Length _ -> Iter.empty

  let fix_cmd env = function
    | Clear i         -> Iter.map (fun i -> Clear i        ) (Env.valid_t_vars env i)
    | Copy i          -> Iter.map (fun i -> Copy i         ) (Env.valid_t_vars env i)
    | Add (i,x,y)     -> Iter.map (fun i -> Add (i,x,y)    ) (Env.valid_t_vars env i)
    | Remove (i,x)    -> Iter.map (fun i -> Remove (i,x)   ) (Env.valid_t_vars env i)
    | Find (i,x)      -> Iter.map (fun i -> Find (i,x)     ) (Env.valid_t_vars env i)
    | Find_opt (i,x)  -> Iter.map (fun i -> Find_opt (i,x) ) (Env.valid_t_vars env i)
    | Find_all (i,x)  -> Iter.map (fun i -> Find_all (i,x) ) (Env.valid_t_vars env i)
    | Replace (i,x,y) -> Iter.map (fun i -> Replace (i,x,y)) (Env.valid_t_vars env i)
    | Mem (i,x)       -> Iter.map (fun i -> Mem (i,x)      ) (Env.valid_t_vars env i)
    | Length i        -> Iter.map (fun i -> Length i       ) (Env.valid_t_vars env i)

  type res =
    | RClear
    | RCopy
    | RAdd
    | RRemove
    | RFind of ((int, exn) result [@equal (=)])
    | RFind_opt of int option
    | RFind_all of int list
    | RReplace
    | RMem of bool
    | RLength of int [@@deriving show { with_path = false }, eq]

  let init () = Hashtbl.create ~random:false 42

  let run c h = match c with
    | None, Clear w         -> Hashtbl.clear h.(w); RClear
    | Some r, Copy w        -> let tmp = Hashtbl.copy h.(w) in h.(r) <- tmp; RCopy
    | None, Add (w,k,v)     -> Hashtbl.add h.(w) k v; RAdd
    | None, Remove (w,k)    -> Hashtbl.remove h.(w) k; RRemove
    | None, Find (w,k)      -> RFind (Util.protect (fun () -> Hashtbl.find h.(w) k) ())
    | None, Find_opt (w,k)  -> RFind_opt (Hashtbl.find_opt h.(w) k)
    | None, Find_all (w,k)  -> RFind_all (Hashtbl.find_all h.(w) k)
    | None, Replace (w,k,v) -> Hashtbl.replace h.(w) k v; RReplace
    | None, Mem (w,k)       -> RMem (Hashtbl.mem h.(w) k)
    | None, Length w        -> RLength (Hashtbl.length h.(w))
    | _, _ -> failwith (Printf.sprintf "unexpected command: %s" (show_cmd (snd c)))

  let cleanup _ = ()
end

module HT = Lin.Make(HConf)
;;
Util.set_ci_printing ()
;;
QCheck_base_runner.run_tests_main [
  HT.neg_lin_test     `Domain ~count:1000 ~name:"Lin Hashtbl test with Domain";
]
