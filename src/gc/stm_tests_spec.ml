open QCheck
open STM

type cmd =
  | Set_minor_heap_size of int
  | Compact
  (* cmds to allocate memory *)
  | PreAllocList of int * char list
  | RevList of int

let pp_cmd par fmt x =
  let open Util.Pp in
  match x with
  | Set_minor_heap_size i -> cst1 pp_int "Set minor_heap_size" par fmt i
  | Compact     -> cst0 "Compact" fmt
  | PreAllocList (i,l) -> cst2 pp_int (pp_list pp_char) "PreAllocList" par fmt i l
  | RevList i   -> cst1 pp_int "RevList" par fmt i

let show_cmd = Util.Pp.to_show pp_cmd

type state = unit

let init_state = ()

let orig_control = Gc.get ()

let array_length = 8

let alloc_cmds, gc_cmds =
  let minor_heap_size_gen = Gen.oneofl [512;1024;2048;4096;8192;16384;32768] in
  let list_gen = Gen.map (fun l -> List.init l (fun _ -> 'l')) Gen.nat in
  let index_gen = Gen.int_bound (array_length-1) in
  let alloc_cmds =
    Gen.([
        (* allocating cmds to activate the Gc *)
        5, map2 (fun index list -> PreAllocList (index,list)) index_gen list_gen;
        5, map (fun index -> RevList index) index_gen;
      ]) in
  let gc_cmds =
    Gen.([
        1, map (fun i -> Set_minor_heap_size i) minor_heap_size_gen;
        1, return Compact;
      ]) @ alloc_cmds in
  alloc_cmds, gc_cmds

let arb_cmd _s = QCheck.make ~print:show_cmd (Gen.frequency gc_cmds)

let arb_alloc_cmd _s = QCheck.make ~print:show_cmd (Gen.frequency alloc_cmds)

let next_state _n _s = ()

type sut = { mutable lists : char list array; }

let init_sut () = { lists = Array.make array_length []; }

let cleanup sut =
  begin
    sut.lists <- [| |];
    Gc.set orig_control;
    Gc.full_major ()
  end

let precond _n _s = true

let run c sut = match c with
  | Set_minor_heap_size i -> Res (unit, Gc.set { orig_control with minor_heap_size = i; })
  | Compact     -> Res (unit, Gc.compact ())
  | PreAllocList (i,l) -> Res (unit, sut.lists.(i) <- l) (*alloc list in parent domain in test-input*)
  | RevList i -> Res (unit, sut.lists.(i) <- List.rev sut.lists.(i)) (*alloc list at test runtime*)

let postcond n (_s: state) res = match n, res with
  | Set_minor_heap_size _, Res ((Unit,_), ()) -> true
  | Compact,    Res ((Unit,_), ()) -> true
  | PreAllocList _, Res ((Unit,_), ()) -> true
  | RevList _,  Res ((Unit,_), ()) -> true
  | _, _ -> false
