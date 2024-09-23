open QCheck
open STM

(* sequential and parallel tests of the GC *)

(* TODO:
   - support OCAMLRUNPARAM / ... in init_state
   - add bigarray
   - support allocations in both parent and child domains
   - split into an implicit and an explicit Gc test
 *)

module GCConf =
struct
  type setcmd =
    | Minor_heap_size of int
    | Major_heap_increment of int (* 1: "This field is currently not available in OCaml 5: the field value is always [0]." *)
    | Space_overhead of int
    (* | Verbose *)
    | Max_overhead of int         (* 4: "This field is currently not available in OCaml 5: the field value is always [0]." *)
    | Stack_limit of int
    (* | Allocation_policy *)     (* 6: "This field is currently not available in OCaml 5: the field value is always [0]." *)
    (* | Window_size of int *)    (* 7: "This field is currently not available in OCaml 5: the field value is always [0]." *)
    | Custom_major_ratio of int
    | Custom_minor_ratio of int
    | Custom_minor_max_size of int

  type cmd =
    | Stat
    | Quick_stat
    | Counters
    | Minor_words
    | Get
    | Set of setcmd
    | Minor
    | Major_slice of int
    | Major
    | Full_major
    | Compact
    | Allocated_bytes
    | Get_minor_free
    (* cmds to allocate memory *)
    | Cons64 of int
    | AllocStr of int * int
    | CatStr of int * int * int
    | AllocList of int * int
    | RevList of int

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Stat        -> cst0 "Stat" fmt
    | Quick_stat  -> cst0 "Quick_stat" fmt
    | Counters    -> cst0 "Counters" fmt
    | Minor_words -> cst0 "Minor_words" fmt
    | Get         -> cst0 "Get" fmt
    | Set subcmd -> (match subcmd with
        | Minor_heap_size i       -> cst1 pp_int "Set minor_heap_size" par fmt i
        | Major_heap_increment i  -> cst1 pp_int "Set major_heap_increment" par fmt i
        | Space_overhead i        -> cst1 pp_int "Set space_overhead" par fmt i
        | Max_overhead i          -> cst1 pp_int "Set max_overhead" par fmt i
        | Stack_limit i           -> cst1 pp_int "Set stack_limit" par fmt i
        | Custom_major_ratio i    -> cst1 pp_int "Set custom_major_ratio" par fmt i
        | Custom_minor_ratio i    -> cst1 pp_int "Set custom_minor_ratio" par fmt i
        | Custom_minor_max_size i -> cst1 pp_int "Set custom_minor_max_size" par fmt i
      )
    | Minor       -> cst0 "Minor" fmt
    | Major_slice n -> cst1 pp_int "Major_slice" par fmt n
    | Major       -> cst0 "Major" fmt
    | Full_major  -> cst0 "Full_major" fmt
    | Compact     -> cst0 "Compact" fmt
    | Allocated_bytes -> cst0 "Allocated_bytes" fmt
    | Get_minor_free -> cst0 "Get_minor_free" fmt
    | Cons64 i    -> cst1 pp_int "Cons64" par fmt i
    | AllocStr (i,l) -> cst2 pp_int pp_int "AllocStr" par fmt i l
    | CatStr (s1,s2,t) -> cst3 pp_int pp_int pp_int "CatStr" par fmt s1 s2 t
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
  let init_state = default_control
  (* try Sys.getenv "OCAMLRUNPARAM"
         with  FIXME *)
  let array_length = 8

  let arb_cmd _s =
    let minor_heap_size_gen = Gen.oneofl [512;1024;2048;4096;8192;16384;32768] in
    let _major_heap_increment = Gen.oneof [Gen.int_bound 100;        (* percentage increment *)
                                          Gen.int_range 101 1000;   (* percentage increment *)
                                          Gen.int_range 1000 10000; (* word increment *)
                                         ] in
    let space_overhead = Gen.int_range 20 200 in   (* percentage increment *)
    let _max_overhead = Gen.oneof [Gen.return 0; (* "If max_overhead is set to 0, heap compaction is triggered at the end of each major GC cycle" *)
                                  Gen.int_range 1 1000;
                                  Gen.return 1_000_000; ] in (* "If max_overhead >= 1000000 , compaction is never triggered." *)
    let stack_limit = Gen.int_range 3284 1_000_000 in
    let custom_major_ratio = Gen.int_range 1 100 in
    let custom_minor_ratio = Gen.int_range 1 100 in
    let custom_minor_max_size = Gen.int_range 10 1_000_000 in
    let int_gen = Gen.small_nat in
    let str_len_gen = Gen.(map (fun shift -> 1 lsl (shift-1)) (int_bound 14)) in (*[-1;13] ~ [0;1;...4096;8196] *)
    let index_gen = Gen.int_bound (array_length-1) in
    QCheck.make ~print:show_cmd
      Gen.(frequency
           (let gens =
             [ 1, return Stat;
               1, return Quick_stat;
               1, return Minor_words;
               10, return Get;
               1, map (fun i -> Set (Minor_heap_size i)) minor_heap_size_gen;
             (*1, map (fun i -> Set (Major_heap_increment i)) major_heap_increment;*)
               1, map (fun i -> Set (Space_overhead i)) space_overhead;
             (*1, map (fun i -> Set (Max_overhead i)) max_overhead;*)
               1, map (fun i -> Set (Stack_limit i)) stack_limit;
               1, map (fun i -> Set (Custom_major_ratio i)) custom_major_ratio;
               1, map (fun i -> Set (Custom_minor_ratio i)) custom_minor_ratio;
               1, map (fun i -> Set (Custom_minor_max_size i)) custom_minor_max_size;
               1, return Minor;
               1, map (fun i -> Major_slice i) Gen.nat; (* "n is the size of the slice: the GC will do enough work to free (on average) n words of memory." *)
               1, return (Major_slice 0); (* cornercase: "If n = 0, the GC will try to do enough work to ensure that the next automatic slice has no work to do" *)
               1, return Major;
               1, return Full_major;
               1, return Compact;
               1, return Allocated_bytes;
               1, return Get_minor_free;
               10, map (fun i -> Cons64 i) int_gen;
               10, map2 (fun index len -> AllocStr (index,len)) index_gen str_len_gen;
               5, map3 (fun src1 src2 tgt -> CatStr (src1,src2,tgt)) index_gen index_gen index_gen;
               10, map2 (fun index len -> AllocList (index,len)) index_gen Gen.nat;
               10, map (fun index -> RevList index) index_gen;
             ] in
           if Sys.(ocaml_release.major,ocaml_release.minor) > (5,3)
           then (1, return Counters)::gens  (* known problem with Counters on <= 5.2: https://github.com/ocaml/ocaml/pull/13370 *)
           else gens))

  let next_state n s = match n with
    | Stat        -> s
    | Quick_stat  -> s
    | Counters    -> s
    | Minor_words -> s
    | Get         -> s
    | Set subcmd -> (match subcmd with
        | Minor_heap_size mhs       -> { s with Gc.minor_heap_size = mhs }
        | Major_heap_increment _mhi -> s (* "This field is currently not available in OCaml 5: the field value is always [0]." *)
        | Space_overhead so         -> { s with Gc.space_overhead = so }
        | Max_overhead _mo          -> s (* "This field is currently not available in OCaml 5: the field value is always [0]." *)
        | Stack_limit sl            -> { s with Gc.stack_limit = sl }
        | Custom_major_ratio cmr    -> { s with Gc.custom_major_ratio = cmr }
        | Custom_minor_ratio cmr    -> { s with Gc.custom_minor_ratio = cmr }
        | Custom_minor_max_size ms  -> { s with Gc.custom_minor_max_size = ms }
      )
    | Minor       -> s
    | Major_slice _ -> s
    | Major       -> s
    | Full_major  -> s
    | Compact     -> s
    | Allocated_bytes -> s
    | Get_minor_free -> s
    | Cons64 _    -> s
    | AllocStr _  -> s
    | CatStr _    -> s
    | AllocList _ -> s
    | RevList _   -> s

(*
BUG
...
   Set stack_limit 3283 : ()
...
  Get : { minor_heap_size = 8192; major_heap_increment = 0; space_overhead = 183; verbose = 0; max_overhead = 0; stack_limit = 3284; allocation_policy = 0; window_size = 0; custom_major_ratio = 44; custom_minor_ratio = 100; custom_minor_max_size = 70000 }

calls 'caml_change_max_stack_size' in runtime/fiber.c:70 which may expand the size slightly it see

also `caml_maybe_expand_stack` may do so
*)
  type sut =
    { mutable int64s  : int64 list;
      mutable strings : string array;
      mutable lists   : char list array; }
  let init_sut () =
    { int64s = [];
      strings = Array.make array_length "";
      lists   = Array.make array_length [];
    }

  let cleanup sut =
    begin
      sut.int64s <- [];
      sut.strings <- [| |];
      sut.lists <- [| |];
      Gc.set default_control;
      Gc.compact ()
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
    | Counters    -> Res (tup3 float float float, Gc.counters ())
    | Minor_words -> Res (float, Gc.minor_words ())
    | Get         -> Res (gccontrol, Gc.get ())
    | Set subcmd -> (match subcmd with
        | Minor_heap_size i       -> Res (unit, let prev = Gc.get () in Gc.set { prev with minor_heap_size = i; })
        | Major_heap_increment i  -> Res (unit, let prev = Gc.get () in Gc.set { prev with major_heap_increment = i; })
        | Space_overhead i        -> Res (unit, let prev = Gc.get () in Gc.set { prev with space_overhead = i; })
        | Max_overhead i          -> Res (unit, let prev = Gc.get () in Gc.set { prev with max_overhead = i; })
        | Stack_limit i           -> Res (unit, let prev = Gc.get () in Gc.set { prev with stack_limit = i; })
        | Custom_major_ratio i    -> Res (unit, let prev = Gc.get () in Gc.set { prev with custom_major_ratio = i; })
        | Custom_minor_ratio i    -> Res (unit, let prev = Gc.get () in Gc.set { prev with custom_minor_ratio = i; })
        | Custom_minor_max_size i -> Res (unit, let prev = Gc.get () in Gc.set { prev with custom_minor_max_size = i; })
      )
    | Minor       -> Res (unit, Gc.minor ())
    | Major_slice n -> Res (int, Gc.major_slice n)
    | Major       -> Res (unit, Gc.major ())
    | Full_major  -> Res (unit, Gc.full_major ())
    | Compact     -> Res (unit, Gc.compact ())
    | Allocated_bytes -> Res (float, Gc.allocated_bytes ())
    | Get_minor_free -> Res (int, Gc.get_minor_free ())
    | Cons64 i    -> Res (unit, sut.int64s <- ((Int64.of_int i)::sut.int64s)) (*alloc int64 and cons cell at test runtime*)
    | AllocStr (i,len) -> Res (unit, sut.strings.(i) <- String.make len 'c') (*alloc string at test runtime*)
    | CatStr (src1,src2,tgt) -> Res (unit, sut.strings.(tgt) <- String.cat sut.strings.(src1) sut.strings.(src2))
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
    | Stat, Res ((GcStat,_),r) -> check_gc_stats r
    | Quick_stat, Res ((GcStat,_),r) -> check_gc_stats r
    | Counters, Res ((Tup3 (Float,Float,Float),_),r) ->
      let (minor_words, promoted_words, major_words) = r in
      minor_words >= 0. && promoted_words >= 0. && major_words >= 0.
    | Minor_words, Res ((Float,_),r) -> r >= 0.
    | Get,         Res ((GcControl,_),r) ->
      (* model-agreement modulo stack_limit which may have been expanded *)
      r = { s with stack_limit = r.Gc.stack_limit } &&
      r.Gc.stack_limit >= s.Gc.stack_limit
    | Set _,      Res ((Unit,_), ()) -> true
    | Minor,      Res ((Unit,_), ()) -> true
    | Major_slice _, Res ((Int,_),r) -> r = 0
    | Major,      Res ((Unit,_), ()) -> true
    | Full_major, Res ((Unit,_), ()) -> true
    | Compact,    Res ((Unit,_), ()) -> true
    | Allocated_bytes, Res ((Float,_),r) -> r >= 0.
    | Get_minor_free, Res ((Int,_),r) -> r >= 0
    | Cons64 _,   Res ((Unit,_), ()) -> true
    | AllocStr _, Res ((Unit,_), ()) -> true
    | CatStr _,  Res ((Unit,_), ()) -> true
    | AllocList _, Res ((Unit,_), ()) -> true
    | RevList _,  Res ((Unit,_), ()) -> true
    | _, _ -> false
end

module GC_STM_seq = STM_sequential.Make(GCConf)
module GC_STM_dom = STM_domain.Make(GCConf)

let agree_prop cs = match Util.protect GC_STM_seq.agree_prop cs with
  | Ok r -> r
  | Error Stack_overflow -> true (* Stack_overflow is accepted behaviour *)
  | Error e -> raise e

(* Run seq. property in a child domain to stresstest parent-child GC *)
let agree_child_prop cs = match Domain.spawn (fun () -> Util.protect GC_STM_seq.agree_prop cs) |> Domain.join with
  | Ok r -> r
  | Error Stack_overflow -> true (* Stack_overflow is accepted behaviour *)
  | Error e -> raise e

let agree_test ~count ~name =
  Test.make ~name ~count (GC_STM_seq.arb_cmds GCConf.init_state) agree_prop

let agree_child_test ~count ~name =
  Test.make ~name ~count (GC_STM_seq.arb_cmds GCConf.init_state) agree_child_prop

let _ =
  QCheck_base_runner.run_tests_main [
    agree_test                    ~count:1000 ~name:"STM Gc test sequential";
    agree_child_test              ~count:1000 ~name:"STM Gc test sequential in child domain";
    GC_STM_dom.neg_agree_test_par ~count:1000 ~name:"STM Gc test parallel";
    GC_STM_dom.stress_test_par    ~count:1000 ~name:"STM Gc stress test parallel";
  ]
