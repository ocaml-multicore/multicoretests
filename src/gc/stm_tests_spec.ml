open QCheck
open STM

type setcmd =
  | Minor_heap_size of int
  | Space_overhead of int
  | Custom_major_ratio of int
  | Custom_minor_ratio of int
  | Custom_minor_max_size of int

type cmd =
  | Get
  | Set of setcmd
  | Minor
  | Full_major
  | Compact
(*| Allocated_bytes*)
  (* cmds to allocate memory *)
  | PreAllocStr of int * string
  | AllocStr of int * int
  | CatStr of int * int * int
  | PreAllocList of int * char list
  | AllocList of int * int
  | RevList of int

let pp_cmd par fmt x =
  let open Util.Pp in
  match x with
  | Get         -> cst0 "Get" fmt
  | Set subcmd -> (match subcmd with
      | Minor_heap_size i       -> cst1 pp_int "Set minor_heap_size" par fmt i
      | Space_overhead i        -> cst1 pp_int "Set space_overhead" par fmt i
      | Custom_major_ratio i    -> cst1 pp_int "Set custom_major_ratio" par fmt i
      | Custom_minor_ratio i    -> cst1 pp_int "Set custom_minor_ratio" par fmt i
      | Custom_minor_max_size i -> cst1 pp_int "Set custom_minor_max_size" par fmt i
    )
  | Minor       -> cst0 "Minor" fmt
  | Full_major  -> cst0 "Full_major" fmt
  | Compact     -> cst0 "Compact" fmt
(*| Allocated_bytes -> cst0 "Allocated_bytes" fmt*)
  | PreAllocStr (i,s) -> cst2 pp_int pp_string "PreAllocStr" par fmt i s
  | AllocStr (i,l) -> cst2 pp_int pp_int "AllocStr" par fmt i l
  | CatStr (s1,s2,t) -> cst3 pp_int pp_int pp_int "CatStr" par fmt s1 s2 t
  | PreAllocList (i,l) -> cst2 pp_int (pp_list pp_char) "PreAllocList" par fmt i l
  | AllocList (i,l) -> cst2 pp_int pp_int "AllocList" par fmt i l
  | RevList i   -> cst1 pp_int "RevList" par fmt i

let show_cmd = Util.Pp.to_show pp_cmd

let default_control = Gc.{
    minor_heap_size = 262_144;      (* Default: 256k. *)
    major_heap_increment = 0;       (* Default: https://github.com/ocaml/ocaml/pull/13440 *)
    space_overhead = 120;           (* Default: 120. *)
    verbose = 0;                    (* Default: 0. *)
    max_overhead = 0;               (* Default: https://github.com/ocaml/ocaml/pull/13440 *)
    stack_limit = 134_217_728;      (* Default: 128M. https://github.com/ocaml/ocaml/pull/13440 *)
    allocation_policy = 0;          (* "This option is ignored in OCaml 5.x." *)
    window_size = 0;                (* Default: https://github.com/ocaml/ocaml/pull/13440 *)
    custom_major_ratio = 44;        (* Default: 44. *)
    custom_minor_ratio = 100;       (* Default: 100. *)
    custom_minor_max_size = 70_000; (* Default: 70000 bytes. *)
  }

type state = Gc.control

let page_size =
  let bytes_per_word = Sys.word_size / 8 in (* bytes per word *)
  Pagesize.get () / bytes_per_word (* page size in words *)

let round_heap_size i =
  if i mod page_size > 0
  then page_size * (1 + (i / page_size))
  else i

(* Non-pretty OCAMLRUNPARAM parsing code *)
let parse_params params = (* "l=2M,b,m=55,M=50,n=50,s=4k,o=75" *)
  let parse_pair s =
    (match String.split_on_char '=' s with
     | [lhs;rhs] -> Some (lhs, rhs)
     | _ -> None) in
  let convert_rhs rhs =
    if rhs="" then None else
      let len = String.length rhs in
      (match rhs.[len - 1] with
       | 'k' -> Some ((1 lsl 10) * int_of_string (String.sub rhs 0 (len - 1)))
       | 'M' -> Some ((1 lsl 20) * int_of_string (String.sub rhs 0 (len - 1)))
       | 'G' -> Some ((1 lsl 30) * int_of_string (String.sub rhs 0 (len - 1)))
       | c ->
         if '0' <= c && c <= '9'
         then Some (int_of_string rhs)
         else None) in
  let param_list = String.split_on_char ',' params in
  let pairs =
    List.fold_right
      (fun s acc -> match parse_pair s with None -> acc | Some pair -> pair::acc)
      param_list [] in
  let num_pairs =
    List.fold_right
      (fun (lhs,rhs) acc -> match convert_rhs rhs with None -> acc | Some num -> (lhs,num)::acc)
      pairs [] in
  num_pairs

let rec interpret_params paramlist s =
  match paramlist with
  | [] -> s
  | pair::ps ->
    let s' = match pair with (* FIXME: The multiplier is k, M, or G, for multiplication by 2^10, 2^20, and 2^30 respectively.*)
      | ("l",sl)  -> { s with Gc.stack_limit = sl }
      | ("m",cmr) -> { s with Gc.custom_minor_ratio = cmr }
      | ("M",cmr) -> { s with Gc.custom_major_ratio = cmr }
      | ("n",cms) -> { s with Gc.custom_minor_max_size = cms }
      | ("o",so)  -> { s with Gc.space_overhead = so }
      | ("s",hs)  -> { s with Gc.minor_heap_size = round_heap_size hs }
      | ("v",vs)  -> { s with Gc.verbose = vs }
      | _ -> s in
    interpret_params ps s'

let init_state =
  let control =
    if Sys.runtime_variant () = "d"
    then { default_control with Gc.verbose = 63 } (* -runtime-variant=d causes verbose=63 *)
    else default_control in
  let params =
    try Sys.getenv "OCAMLRUNPARAM" with Not_found ->
    try Sys.getenv "CAMLRUNPARAM" with Not_found -> "" in
  interpret_params (parse_params params) control

let array_length = 8

let alloc_cmds, gc_cmds =
  let minor_heap_size_gen = Gen.oneofl [512;1024;2048;4096;8192;16384;32768] in
  let _major_heap_increment = Gen.oneof [Gen.int_bound 100;        (* percentage increment *)
                                         Gen.int_range 101 1000;   (* percentage increment *)
                                         Gen.int_range 1000 10000; (* word increment *)
                                        ] in
  let space_overhead = Gen.int_range 20 200 in   (* percentage increment *)
  let _max_overhead = Gen.oneof [Gen.return 0; (* "If max_overhead is set to 0, heap compaction is triggered at the end of each major GC cycle" *)
                                 Gen.int_range 1 1000;
                                 Gen.return 1_000_000; ] in (* "If max_overhead >= 1000000 , compaction is never triggered." *)
  let custom_major_ratio = Gen.int_range 1 100 in
  let custom_minor_ratio = Gen.int_range 1 100 in
  let custom_minor_max_size = Gen.int_range 10 1_000_000 in
  let str_len_gen = Gen.(map (fun shift -> 1 lsl (shift-1)) (int_bound 14)) in (*[-1;13] ~ [0;1;...4096;8196] *)
  let str_gen = Gen.map (fun l -> String.make l 'x') str_len_gen in
  let list_gen = Gen.map (fun l -> List.init l (fun _ -> 'l')) Gen.nat in
  let index_gen = Gen.int_bound (array_length-1) in
  let alloc_cmds =
    Gen.([
        (* purely observational cmds *)
        5, return Get;
      (*1, return Allocated_bytes;*)
        (* allocating cmds to activate the Gc *)
        5, map2 (fun index str -> PreAllocStr (index,str)) index_gen str_gen;
        5, map2 (fun index len -> AllocStr (index,len)) index_gen str_len_gen;
        5, map3 (fun src1 src2 tgt -> CatStr (src1,src2,tgt)) index_gen index_gen index_gen;
        5, map2 (fun index list -> PreAllocList (index,list)) index_gen list_gen;
        5, map2 (fun index len -> AllocList (index,len)) index_gen Gen.nat;
        5, map (fun index -> RevList index) index_gen;
      ]) in
  let gc_cmds =
    Gen.([
        1, map (fun i -> Set (Minor_heap_size i)) minor_heap_size_gen;
        1, map (fun i -> Set (Space_overhead i)) space_overhead;
        1, map (fun i -> Set (Custom_major_ratio i)) custom_major_ratio;
        1, map (fun i -> Set (Custom_minor_ratio i)) custom_minor_ratio;
        1, map (fun i -> Set (Custom_minor_max_size i)) custom_minor_max_size;
        1, return Minor;
        1, return Full_major;
        1, return Compact;
      ]) @ alloc_cmds in
  alloc_cmds, gc_cmds

let arb_cmd _s = QCheck.make ~print:show_cmd (Gen.frequency gc_cmds)

let arb_alloc_cmd _s = QCheck.make ~print:show_cmd (Gen.frequency alloc_cmds)

let next_state n s = match n with
  | Get         -> s
  | Set subcmd -> (match subcmd with
      | Minor_heap_size mhs       -> { s with Gc.minor_heap_size = round_heap_size mhs }
      | Space_overhead so         -> { s with Gc.space_overhead = so }
      | Custom_major_ratio cmr    -> { s with Gc.custom_major_ratio = cmr }
      | Custom_minor_ratio cmr    -> { s with Gc.custom_minor_ratio = cmr }
      | Custom_minor_max_size ms  -> { s with Gc.custom_minor_max_size = ms }
    )
  | Minor       -> s
  | Full_major  -> s
  | Compact     -> s
(*| Allocated_bytes -> s*)
  | PreAllocStr _ -> s
  | AllocStr _  -> s
  | CatStr _    -> s
  | PreAllocList _ -> s
  | AllocList _ -> s
  | RevList _   -> s

type sut =
  { mutable strings : string array;
    mutable lists   : char list array;
  }
let init_sut () =
  { strings = Array.make array_length "";
    lists   = Array.make array_length [];
  }

let cleanup sut =
  begin
    sut.strings <- [| |];
    sut.lists <- [| |];
    Gc.set init_state;
    Gc.full_major ()
  end

let precond _n _s = true

type _ ty += Tup3 : 'a ty * 'b ty * 'c ty -> ('a * 'b * 'c) ty
          | GcStat: Gc.stat ty
          | GcControl: Gc.control ty

let tup3 spec_a spec_b spec_c =
  let (ty_a,show_a) = spec_a in
  let (ty_b,show_b) = spec_b in
  let (ty_c,show_c) = spec_c in
  (Tup3 (ty_a,ty_b,ty_c), QCheck.Print.tup3 show_a show_b show_c)

let pp_gcstat par fmt s =
  let open Util.Pp in
  pp_record par fmt
    [
      pp_field "minor_words" pp_float s.Gc.minor_words;
      pp_field "promoted_words" pp_float s.Gc.promoted_words;
      pp_field "major_words" pp_float s.Gc.major_words;
      pp_field "minor_collections" pp_int s.Gc.minor_collections;
      pp_field "major_collections" pp_int s.Gc.major_collections;
      pp_field "heap_words" pp_int s.Gc.heap_words;
      pp_field "heap_chunks" pp_int s.Gc.heap_chunks;
      pp_field "live_words" pp_int s.Gc.live_words;
      pp_field "live_blocks" pp_int s.Gc.live_blocks;
      pp_field "free_words" pp_int s.Gc.free_words;
      pp_field "free_blocks" pp_int s.Gc.free_blocks;
      pp_field "largest_free" pp_int s.Gc.largest_free;
      pp_field "fragments" pp_int s.Gc.fragments;
      pp_field "compactions" pp_int s.Gc.compactions;
      pp_field "top_heap_words" pp_int s.Gc.top_heap_words;
      pp_field "stack_size" pp_int s.Gc.stack_size;
      pp_field "forced_major_collections" pp_int s.Gc.forced_major_collections;
    ]

let show_gcstat = Util.Pp.to_show pp_gcstat

let gcstat = (GcStat, show_gcstat)

let pp_gccontrol par fmt c =
  let open Util.Pp in
  pp_record par fmt
    [
      pp_field "minor_heap_size" pp_int c.Gc.minor_heap_size;
      pp_field "major_heap_increment" pp_int c.Gc.major_heap_increment;
      pp_field "space_overhead" pp_int c.Gc.space_overhead;
      pp_field "verbose" pp_int c.Gc.verbose;
      pp_field "max_overhead" pp_int c.Gc.max_overhead;
      pp_field "stack_limit" pp_int c.Gc.stack_limit;
      pp_field "allocation_policy" pp_int c.Gc.allocation_policy;
      pp_field "window_size" pp_int c.Gc.window_size;
      pp_field "custom_major_ratio" pp_int c.Gc.custom_major_ratio;
      pp_field "custom_minor_ratio" pp_int c.Gc.custom_minor_ratio;
      pp_field "custom_minor_max_size" pp_int c.Gc.custom_minor_max_size;
    ]

let show_gccontrol = Util.Pp.to_show pp_gccontrol

let gccontrol = (GcControl, show_gccontrol)

let run c sut = match c with
  | Get         -> Res (gccontrol, Gc.get ())
  | Set subcmd -> (match subcmd with
      | Minor_heap_size i       -> Res (unit, let prev = Gc.get () in Gc.set { prev with minor_heap_size = i; })
      | Space_overhead i        -> Res (unit, let prev = Gc.get () in Gc.set { prev with space_overhead = i; })
      | Custom_major_ratio i    -> Res (unit, let prev = Gc.get () in Gc.set { prev with custom_major_ratio = i; })
      | Custom_minor_ratio i    -> Res (unit, let prev = Gc.get () in Gc.set { prev with custom_minor_ratio = i; })
      | Custom_minor_max_size i -> Res (unit, let prev = Gc.get () in Gc.set { prev with custom_minor_max_size = i; })
    )
  | Minor       -> Res (unit, Gc.minor ())
  | Full_major  -> Res (unit, Gc.full_major ())
  | Compact     -> Res (unit, Gc.compact ())
(*| Allocated_bytes -> Res (float, Gc.allocated_bytes ())*)
  | PreAllocStr (i,s) -> Res (unit, sut.strings.(i) <- s) (*alloc string in parent domain in test-input*)
  | AllocStr (i,len) -> Res (unit, sut.strings.(i) <- String.make len 'c') (*alloc string at test runtime*)
  | CatStr (src1,src2,tgt) -> Res (unit, sut.strings.(tgt) <- String.cat sut.strings.(src1) sut.strings.(src2))
  | PreAllocList (i,l) -> Res (unit, sut.lists.(i) <- l) (*alloc list in parent domain in test-input*)
  | AllocList (i,len) -> Res (unit, sut.lists.(i) <- List.init len (fun _ -> 'a')) (*alloc list at test runtime*)
  | RevList i -> Res (unit, sut.lists.(i) <- List.rev sut.lists.(i)) (*alloc list at test runtime*)

let check_gc_stats r =
  r.Gc.minor_words >= 0. &&
  r.Gc.promoted_words >= 0. &&
  r.Gc.major_words >= 0. &&
  r.Gc.minor_collections >= 0 &&
  r.Gc.major_collections >= 0 &&
  r.Gc.heap_words >= 0 &&
  r.Gc.heap_chunks = 0 &&  (* Note: currently always 0 in OCaml5 *)
  r.Gc.live_words >= 0 &&  (* https://github.com/ocaml/ocaml/pull/13424 *)
  r.Gc.live_blocks >= 0 && (* https://github.com/ocaml/ocaml/pull/13424 *)
  r.Gc.free_words >= 0 &&  (* https://github.com/ocaml/ocaml/pull/13424 *)
  r.Gc.free_blocks = 0 &&  (* Note: currently always 0 in OCaml5 *)
  r.Gc.largest_free = 0 && (* Note: currently always 0 in OCaml5 *)
  r.Gc.fragments >= 0 &&   (* https://github.com/ocaml/ocaml/pull/13424 *)
  r.Gc.compactions >= 0 &&
  r.Gc.top_heap_words >= 0 &&
  r.Gc.stack_size = 0 &&   (* Note: currently always 0 in OCaml5 *)
  r.Gc.forced_major_collections >= 0

let postcond n (s: state) res = match n, res with
  | Get,         Res ((GcControl,_),r) ->
    (* model-agreement modulo stack_limit which may have been expanded *)
    r = { s with stack_limit = r.Gc.stack_limit } &&
    r.Gc.stack_limit >= s.Gc.stack_limit
  | Set _,      Res ((Unit,_), ()) -> true
  | Minor,      Res ((Unit,_), ()) -> true
  | Full_major, Res ((Unit,_), ()) -> true
  | Compact,    Res ((Unit,_), ()) -> true
(*| Allocated_bytes, Res ((Float,_),r) -> r >= 0.*)
  | PreAllocStr _, Res ((Unit,_), ()) -> true
  | AllocStr _, Res ((Unit,_), ()) -> true
  | CatStr _,  Res ((Unit,_), ()) -> true
  | PreAllocList _, Res ((Unit,_), ()) -> true
  | AllocList _, Res ((Unit,_), ()) -> true
  | RevList _,  Res ((Unit,_), ()) -> true
  | _, _ -> false
