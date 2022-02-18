open QCheck

(** ********************************************************************** *)
(**              Tests of [lf_list] from the lockfree library              *)
(** ********************************************************************** *)
module LFLConf = (* for plain list operations *)
struct
  type t = int Lockfree.List.t

  type cmd =
    | Push of (int [@gen Gen.nat])
    | Pop
    | Is_empty
    | To_string [@@deriving qcheck, show { with_path = false }]

  type res =
    | RPush
    | RPop of int option
    | RIs_empty of bool
    | RTo_string of string [@@deriving show { with_path = false }]

  let dummy = RPush
  
  let init () = Lockfree.List.create ()

  let run c l = match c with
    | Push i    -> (Lockfree.List.push l i; RPush)
    | Pop       -> RPop (Lockfree.List.pop l)
    | Is_empty  -> RIs_empty (Lockfree.List.is_empty l)
    | To_string -> RTo_string (Lockfree.List.to_string l string_of_int)

  let cleanup _ = ()
end

module LFOLConf = (* for ordered list operations *)
struct
  type t = int Lockfree.List.t

  type cmd =
    | Mem of int'
    | Find of int'
    | SInsert of int' [@@deriving qcheck, show { with_path = false }]
  and int' = int [@gen Gen.nat]

  type res =
    | RMem of bool
    | RFind of int option
    | RSInsert of bool (*sut*) [@@deriving show { with_path = false }]

  let dummy = RMem true
  
  let init () = Lockfree.List.create ()

  let run c l = match c with
    | Mem i     -> RMem (Lockfree.List.mem l i Int.compare)
    | Find i    -> RFind (Lockfree.List.find l i Int.compare)
    | SInsert i -> let b,_l' = (Lockfree.List.sinsert l i Int.compare) in
                   RSInsert b (*FIXME l' ignored *)

  let cleanup _ = ()
end

module LFLT = Lin.Make(LFLConf)
module LFOLT = Lin.Make(LFOLConf)


(** ********************************************************************** *)
(**                         Tests of [lockfree.Hash]                       *)
(** ********************************************************************** *)
module LFHConf =
struct
  type t = char Lockfree.Hash.t

  type cmd =
    (*| To_string*) (* of char -> int *) (* we instead pass a meaningful element printer *)
    | Find of int'
    | Mem of int'
    | Add of int' * char'
    | Remove of int'
    | Elem_of [@@deriving qcheck, show { with_path = false }]
  and int' = int [@gen Gen.nat]
  and char' = char [@gen Gen.printable]

  type res =
    (*| RTo_string of string*)
    | RFind of char option
    | RMem of bool
    | RAdd
    | RRemove of bool
    | RElem_of of char list [@@deriving show { with_path = false }]

  let dummy = RFind None
  
  let init () = Lockfree.Hash.create ()

  let run c h = match c with
    (*| To_string -> RTo_string (Lockfree.Hash.to_string h (String.make 1))*)
    | Find k    -> RFind (Lockfree.Hash.find h k)
    | Mem k     -> RMem (Lockfree.Hash.mem h k)
    | Add (k,v) -> Lockfree.Hash.add h k v; RAdd
    | Remove k  -> RRemove (Lockfree.Hash.remove h k)
    | Elem_of   -> RElem_of (Lockfree.Hash.elem_of h)

  let cleanup _ = ()
end

module LFHT = Lin.Make(LFHConf)


(** ********************************************************************** *)
(**                         Tests of [lockfree.Bag]                        *)
(** ********************************************************************** *)
module LFBConf =
struct
  type t = int Lockfree.Bag.t

  type cmd =
    | Is_empty
    | Push of (int [@gen Gen.nat])
    | Pop [@@deriving qcheck, show { with_path = false }]

  type res =
    | RIs_empty of bool
    | RPush
    | RPop of int option [@@deriving show { with_path = false }]

  let dummy = RIs_empty true
  
  let init () = Lockfree.Bag.create ()

  let run c h = match c with
    | Is_empty -> RIs_empty (Lockfree.Bag.is_empty h)
    | Push e   -> Lockfree.Bag.push h e; RPush
    | Pop      -> RPop (Lockfree.Bag.pop h)

  let cleanup _ = ()
end

module LFBT = Lin.Make(LFBConf)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main [
  (* Lockfree tests *)
  LFLT.lin_test   `Domain ~count:1000 ~name:"lockfree list test";
  LFOLT.lin_test  `Domain ~count:1000 ~name:"lockfree ordered list test";
  LFHT.lin_test   `Domain ~count:1000 ~name:"lockfree Hash test";
  LFBT.lin_test   `Domain ~count:1000 ~name:"lockfree Bag test";
]
