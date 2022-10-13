open QCheck

(** ********************************************************************** *)
(**                       Tests of a simple reference                      *)
(** ********************************************************************** *)
module Sut_int =
  struct
    let init () = ref 0
    let get r = !r
    let set r i = r:=i
    let add r i = let old = !r in r:= i + old (* buggy: not atomic *)
    let incr r = incr r                       (* buggy: not atomic *)
    let decr r = decr r                       (* buggy: not atomic *)
end

module Sut_int64 =
  struct
    let init () = ref Int64.zero
    let get r = !r
    let set r i = r:=i
    let add r i = let old = !r in r:= Int64.add i old (* buggy: not atomic *)
    let incr r = add r Int64.one                      (* buggy: not atomic *)
    let decr r = add r Int64.minus_one                (* buggy: not atomic *)
end

module RConf_int = struct
  open Lin

  type t = int ref

  type cmd =
    | Get of Var.t
    | Set of Var.t * int
    | Add of Var.t * int
    | Incr of Var.t
    | Decr of Var.t [@@deriving show { with_path = false }]

  let gen_int = Gen.nat
  let gen_cmd gen_var =
    Gen.(oneof[
        map  (fun t -> None,Get t) gen_var;
        map2 (fun t i -> None,Set (t,i)) gen_var gen_int;
        map2 (fun t i -> None,Add (t,i)) gen_var gen_int;
        map  (fun t -> None,Incr t) gen_var;
        map  (fun t -> None,Decr t) gen_var;
      ])

  let shrink_cmd c = match c with
    | Get _
    | Incr _
    | Decr _ -> Iter.empty
    | Set (t,i) -> Iter.map (fun i -> Set (t,i)) (Shrink.int i)
    | Add (t,i) -> Iter.map (fun i -> Add (t,i)) (Shrink.int i)

  let fix_cmd env = function
    | Get i     -> Iter.map (fun i -> Get i    ) (Env.valid_t_vars env i)
    | Set (i,x) -> Iter.map (fun i -> Set (i,x)) (Env.valid_t_vars env i)
    | Add (i,x) -> Iter.map (fun i -> Add (i,x)) (Env.valid_t_vars env i)
    | Incr i    -> Iter.map (fun i -> Incr i   ) (Env.valid_t_vars env i)
    | Decr i    -> Iter.map (fun i -> Decr i   ) (Env.valid_t_vars env i)

  type res = RGet of int | RSet | RAdd | RIncr | RDecr [@@deriving show { with_path = false }, eq]

  let init () = Sut_int.init ()

  let run c r = match c with
    | None,Get t     -> RGet (Sut_int.get r.(t))
    | None,Set (t,i) -> (Sut_int.set r.(t) i; RSet)
    | None,Add (t,i) -> (Sut_int.add r.(t) i; RAdd)
    | None,Incr t    -> (Sut_int.incr r.(t); RIncr)
    | None,Decr t    -> (Sut_int.decr r.(t); RDecr)
    | _, _ -> failwith (Printf.sprintf "unexpected command: %s" (show_cmd (snd c)))

  let cleanup _ = ()
end


module RConf_int64 = struct
  open Lin

  type t = int64 ref

  type cmd =
    | Get of Var.t
    | Set of Var.t * int64
    | Add of Var.t * int64
    | Incr of Var.t
    | Decr of Var.t [@@deriving show { with_path = false }]

  let gen_int64 = Gen.(map Int64.of_int nat)
  let gen_cmd gen_var =
    Gen.(oneof [
        map  (fun t -> None,Get t) gen_var;
        map2 (fun t i -> None,Set (t,i)) gen_var gen_int64;
        map2 (fun t i -> None,Add (t,i)) gen_var gen_int64;
        map  (fun t -> None,Incr t) gen_var;
        map  (fun t -> None,Decr t) gen_var;
      ])

  let shrink_cmd c = match c with
    | Get _
    | Incr _
    | Decr _ -> Iter.empty
    | Set (t,i) -> Iter.map (fun i -> Set (t,i)) (Shrink.int64 i)
    | Add (t,i) -> Iter.map (fun i -> Add (t,i)) (Shrink.int64 i)

  let fix_cmd env = function
    | Get i     -> Iter.map (fun i -> Get i    ) (Env.valid_t_vars env i)
    | Set (i,x) -> Iter.map (fun i -> Set (i,x)) (Env.valid_t_vars env i)
    | Add (i,x) -> Iter.map (fun i -> Add (i,x)) (Env.valid_t_vars env i)
    | Incr i    -> Iter.map (fun i -> Incr i   ) (Env.valid_t_vars env i)
    | Decr i    -> Iter.map (fun i -> Decr i   ) (Env.valid_t_vars env i)

  type res = RGet of int64 | RSet | RAdd | RIncr | RDecr [@@deriving show { with_path = false }, eq]

  let init () = Sut_int64.init ()

  let run c r = match c with
    | None,Get t     -> RGet (Sut_int64.get r.(t))
    | None,Set (t,i) -> (Sut_int64.set r.(t) i; RSet)
    | None,Add (t,i) -> (Sut_int64.add r.(t) i; RAdd)
    | None,Incr t    -> (Sut_int64.incr r.(t); RIncr)
    | None,Decr t    -> (Sut_int64.decr r.(t); RDecr)
    | _, _ -> failwith (Printf.sprintf "unexpected command: %s" (show_cmd (snd c)))

  let cleanup _ = ()
end

module RT_int = Lin.Make(RConf_int)
module RT_int64 = Lin.Make(RConf_int64)


(** ********************************************************************** *)
(**                  Tests of the buggy concurrent list CList              *)
(** ********************************************************************** *)
module CLConf (T : sig
                     type t
                     val zero : t
                     val of_int : int -> t
                     val to_string : t -> string
                     val shrink : t Shrink.t
                   end) =
struct
  type t = T.t CList.conc_list Atomic.t
  type int' = T.t
  let gen_int' = Gen.(map T.of_int nat)
  let pp_int' fmt t = Format.fprintf fmt "%s" (T.to_string t)

  type cmd =
    | Add_node of Lin.Var.t * int'
    | Member of Lin.Var.t * int' [@@deriving show { with_path = false }]

  let gen_cmd gen_var =
    Gen.(oneof [
        map2 (fun t i -> None,Add_node (t,i)) gen_var gen_int';
        map2 (fun t i -> None,Member (t,i)) gen_var gen_int';
      ])

  let shrink_cmd c = match c with
    | Add_node (t,i) -> Iter.map (fun i -> Add_node (t,i)) (T.shrink i)
    | Member (t,i) -> Iter.map (fun i -> Member (t,i)) (T.shrink i)

  let fix_cmd env = function
    | Add_node (i,x) -> Iter.map (fun i -> Add_node (i,x)) (Lin.Env.valid_t_vars env i)
    | Member (i,x)   -> Iter.map (fun i -> Member (i,x)  ) (Lin.Env.valid_t_vars env i)

  type res = RAdd_node of bool | RMember of bool [@@deriving show { with_path = false }, eq]

  let init () = CList.list_init T.zero

  let run c r = match c with
    | None,Add_node (t,i) -> RAdd_node (CList.add_node r.(t) i)
    | None,Member (t,i)   -> RMember (CList.member r.(t) i)
    | _, _ -> failwith (Printf.sprintf "unexpected command: %s" (show_cmd (snd c)))

  let cleanup _ = ()
end

module Int = struct
  include Stdlib.Int
  let of_int (i:int) : t = i
  let shrink = Shrink.int
end

module Int64 = struct
  include Stdlib.Int64
  let shrink = Shrink.int64
end
module CLT_int = Lin.Make(CLConf (Int))
module CLT_int64 = Lin.Make(CLConf (Int64))
