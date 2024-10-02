open QCheck
open STM

type cmd =
  | Stat
  | Quick_stat
  | Minor_words
  | Get
  | Allocated_bytes
  | Get_minor_free
  (* cmds to allocate memory *)
  | Cons64 of int
  | PreAllocStr of int * string
  | AllocStr of int * int
  | CatStr of int * int * int
  | PreAllocList of int * char list
  | AllocList of int * int
  | RevList of int
  | PreAllocBigarray of int * (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
  | AllocBigarray of int * int

let pp_cmd par fmt x =
  let open Util.Pp in
  match x with
  | Stat        -> cst0 "Stat" fmt
  | Quick_stat  -> cst0 "Quick_stat" fmt
  | Minor_words -> cst0 "Minor_words" fmt
  | Get         -> cst0 "Get" fmt
  | Allocated_bytes -> cst0 "Allocated_bytes" fmt
  | Get_minor_free -> cst0 "Get_minor_free" fmt
  | Cons64 i    -> cst1 pp_int "Cons64" par fmt i
  | PreAllocStr (i,s) -> cst2 pp_int pp_string "PreAllocStr" par fmt i s
  | AllocStr (i,l) -> cst2 pp_int pp_int "AllocStr" par fmt i l
  | CatStr (s1,s2,t) -> cst3 pp_int pp_int pp_int "CatStr" par fmt s1 s2 t
  | PreAllocList (i,l) -> cst2 pp_int (pp_list pp_char) "PreAllocList" par fmt i l
  | AllocList (i,l) -> cst2 pp_int pp_int "AllocList" par fmt i l
  | RevList i   -> cst1 pp_int "RevList" par fmt i
  | PreAllocBigarray (i,_l) -> cst2 pp_int pp_string "AllocBigarray" par fmt i "[|...|]"
  | AllocBigarray (i,l) -> cst2 pp_int pp_int "AllocBigarray" par fmt i l

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
    let s' = match pair with (* "The multiplier is k, M, or G, for multiplication by 2^10, 2^20, and 2^30 respectively." *)
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

let gc_cmds =
  let int_gen = Gen.small_nat in
  let str_len_gen = Gen.(map (fun shift -> 1 lsl (shift-1)) (int_bound 14)) in (*[-1;13] ~ [0;1;...4096;8196] *)
  let str_gen = Gen.map (fun l -> String.make l 'x') str_len_gen in
  let list_gen = Gen.map (fun l -> List.init l (fun _ -> 'l')) Gen.nat in
  let bigarray_gen = Gen.map (fun l -> Bigarray.(Array1.create int C_layout l)) Gen.nat in
  let index_gen = Gen.int_bound (array_length-1) in
  Gen.([
      (* purely observational cmds *)
      1, return Stat;
      1, return Quick_stat;
      1, return Minor_words;
      5, return Get;
      1, return Allocated_bytes;
      1, return Get_minor_free;
      (* allocating cmds to activate the Gc *)
      5, map (fun i -> Cons64 i) int_gen;
      5, map2 (fun index str -> PreAllocStr (index,str)) index_gen str_gen;
      5, map2 (fun index len -> AllocStr (index,len)) index_gen str_len_gen;
      5, map3 (fun src1 src2 tgt -> CatStr (src1,src2,tgt)) index_gen index_gen index_gen;
      5, map2 (fun index list -> PreAllocList (index,list)) index_gen list_gen;
      5, map2 (fun index len -> AllocList (index,len)) index_gen Gen.nat;
      5, map (fun index -> RevList index) index_gen;
      5, map2 (fun index ba -> PreAllocBigarray (index,ba)) index_gen bigarray_gen;
      5, map2 (fun index len -> AllocBigarray (index,len)) index_gen Gen.nat;
    ])

let arb_cmd _s =
  QCheck.make ~print:show_cmd (Gen.frequency gc_cmds)

let next_state n s = match n with
  | Stat        -> s
  | Quick_stat  -> s
  | Minor_words -> s
  | Get         -> s
  | Allocated_bytes -> s
  | Get_minor_free -> s
  | Cons64 _    -> s
  | PreAllocStr _ -> s
  | AllocStr _  -> s
  | CatStr _    -> s
  | PreAllocList _ -> s
  | AllocList _ -> s
  | RevList _   -> s
  | PreAllocBigarray _ -> s
  | AllocBigarray _ -> s

type sut =
  { mutable int64s  : int64 list;
    mutable strings : string array;
    mutable lists   : char list array;
    mutable bigarrays : (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t array; }
let init_sut () =
  { int64s = [];
    strings = Array.make array_length "";
    lists   = Array.make array_length [];
    bigarrays = Array.make array_length Bigarray.(Array1.create int C_layout 0);
  }

let cleanup sut =
  begin
    sut.int64s <- [];
    sut.strings <- [| |];
    sut.lists <- [| |];
    sut.bigarrays <- [| |];
    Gc.set init_state;
    Gc.major ()
  end

let precond n _s = match n with
  | _ -> true

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
  | Stat        -> Res (gcstat, Gc.stat ())
  | Quick_stat  -> Res (gcstat, Gc.quick_stat ())
  | Minor_words -> Res (float, Gc.minor_words ())
  | Get         -> Res (gccontrol, Gc.get ())
  | Allocated_bytes -> Res (float, Gc.allocated_bytes ())
  | Get_minor_free -> Res (int, Gc.get_minor_free ())
  | Cons64 i    -> Res (unit, sut.int64s <- ((Int64.of_int i)::sut.int64s)) (*alloc int64 and cons cell at test runtime*)
  | PreAllocStr (i,s) -> Res (unit, sut.strings.(i) <- s) (*alloc string in parent domain in test-input*)
  | AllocStr (i,len) -> Res (unit, sut.strings.(i) <- String.make len 'c') (*alloc string at test runtime*)
  | CatStr (src1,src2,tgt) -> Res (unit, sut.strings.(tgt) <- String.cat sut.strings.(src1) sut.strings.(src2))
  | PreAllocList (i,l) -> Res (unit, sut.lists.(i) <- l) (*alloc list in parent domain in test-input*)
  | AllocList (i,len) -> Res (unit, sut.lists.(i) <- List.init len (fun _ -> 'a')) (*alloc list at test runtime*)
  | RevList i -> Res (unit, sut.lists.(i) <- List.rev sut.lists.(i)) (*alloc list at test runtime*)
  | PreAllocBigarray (i,ba) -> Res (unit, sut.bigarrays.(i) <- ba) (*alloc bigarray in parent domain in test-input*)
  | AllocBigarray (i,len) -> Res (unit, let ba = Bigarray.(Array1.create int C_layout len) in
                                  Bigarray.Array1.fill ba 0xbeef;
                                  sut.bigarrays.(i) <- ba) (*alloc bigarray at test runtime*)

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
  | Stat, Res ((GcStat,_),r) -> check_gc_stats r
  | Quick_stat, Res ((GcStat,_),r) -> check_gc_stats r
  | Minor_words, Res ((Float,_),r) -> r >= 0.
  | Get,         Res ((GcControl,_),r) ->
    (* model-agreement modulo stack_limit which may have been expanded *)
    r = { s with stack_limit = r.Gc.stack_limit } &&
    r.Gc.stack_limit >= s.Gc.stack_limit
  | Allocated_bytes, Res ((Float,_),r) -> r >= 0.
  | Get_minor_free, Res ((Int,_),r) -> r >= 0
  | Cons64 _,   Res ((Unit,_), ()) -> true
  | PreAllocStr _, Res ((Unit,_), ()) -> true
  | AllocStr _, Res ((Unit,_), ()) -> true
  | CatStr _,  Res ((Unit,_), ()) -> true
  | PreAllocList _, Res ((Unit,_), ()) -> true
  | AllocList _, Res ((Unit,_), ()) -> true
  | RevList _,  Res ((Unit,_), ()) -> true
  | PreAllocBigarray _, Res ((Unit,_), ()) -> true
  | AllocBigarray _, Res ((Unit,_), ()) -> true
  | _, _ -> false
