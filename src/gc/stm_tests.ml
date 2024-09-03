open QCheck
open STM

(* sequential and parallel tests of the GC *)

module GCConf =
struct
  type cmd =
    | Counters
    | Minor_words
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
    | AllocList of int * int
    | RevList of int

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Counters    -> cst0 "Counters" fmt
    | Minor_words -> cst0 "Minor_words" fmt
    | Minor       -> cst0 "Minor" fmt
    | Major_slice n -> cst1 pp_int "Major_slice" par fmt n
    | Major       -> cst0 "Major" fmt
    | Full_major  -> cst0 "Full_major" fmt
    | Compact     -> cst0 "Compact" fmt
    | Allocated_bytes -> cst0 "Allocated_bytes" fmt
    | Get_minor_free -> cst0 "Get_minor_free" fmt
    | Cons64 i    -> cst1 pp_int "Cons64" par fmt i
    | AllocStr (i,l) -> cst2 pp_int pp_int "AllocStr" par fmt i l
    | AllocList (i,l) -> cst2 pp_int pp_int "AllocList" par fmt i l
    | RevList i   -> cst1 pp_int "RevList" par fmt i

  let show_cmd = Util.Pp.to_show pp_cmd

  type state = unit
  let init_state  = ()

  let array_length = 8

  let arb_cmd _s =
    let int_gen = Gen.small_nat in
    let str_len_gen = Gen.(map (fun shift -> 1 lsl (shift-1)) (int_bound 14)) in (*[-1;13] ~ [0;1;...4096;8196] *)
    let index_gen = Gen.int_bound (array_length-1) in
    QCheck.make ~print:show_cmd
      Gen.(frequency
             [ 1, return Counters;  (* known problem with Counters on <= 5.2: https://github.com/ocaml/ocaml/pull/13370 *)
               1, return Minor_words;
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
               10, map2 (fun index len -> AllocList (index,len)) index_gen Gen.nat;
               10, map (fun index -> RevList index) index_gen;
             ])

  let next_state n _s = match n with
    | Counters    -> ()
    | Minor_words -> ()
    | Minor       -> ()
    | Major_slice _ -> ()
    | Major       -> ()
    | Full_major  -> ()
    | Compact     -> ()
    | Allocated_bytes -> ()
    | Get_minor_free -> ()
    | Cons64 _    -> ()
    | AllocStr _  -> ()
    | AllocList _ -> ()
    | RevList _   -> ()

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
      Gc.compact ()
    end

  let precond n _s = match n with
    | _ -> true

  type _ ty += Tup3 : 'a ty * 'b ty * 'c ty -> ('a * 'b * 'c) ty

  let tup3 spec_a spec_b spec_c =
    let (ty_a,show_a) = spec_a in
    let (ty_b,show_b) = spec_b in
    let (ty_c,show_c) = spec_c in
    (Tup3 (ty_a,ty_b,ty_c), QCheck.Print.tup3 show_a show_b show_c)

  let run c sut = match c with
    | Counters    -> Res (tup3 float float float, Gc.counters ())
    | Minor_words -> Res (float, Gc.minor_words ())
    | Minor       -> Res (unit, Gc.minor ())
    | Major_slice n -> Res (int, Gc.major_slice n)
    | Major       -> Res (unit, Gc.major ())
    | Full_major  -> Res (unit, Gc.full_major ())
    | Compact     -> Res (unit, Gc.compact ())
    | Allocated_bytes -> Res (float, Gc.allocated_bytes ())
    | Get_minor_free -> Res (int, Gc.get_minor_free ())
    | Cons64 i    -> Res (unit, sut.int64s <- ((Int64.of_int i)::sut.int64s)) (*alloc int64 and cons cell at test runtime*)
    | AllocStr (i,len) -> Res (unit, sut.strings.(i) <- String.make len 'c') (*alloc string at test runtime*)
    | AllocList (i,len) -> Res (unit, sut.lists.(i) <- List.init len (fun _ -> 'a')) (*alloc list at test runtime*)
    | RevList i -> Res (unit, sut.lists.(i) <- List.rev sut.lists.(i)) (*alloc list at test runtime*)

  let postcond n (_s: unit) res = match n, res with
    | Counters, Res ((Tup3 (Float,Float,Float),_),r) ->
      let (minor_words, promoted_words, major_words) = r in
      minor_words >= 0. && promoted_words >= 0. && major_words >= 0.
    | Minor_words, Res ((Float,_),r) -> r >= 0.
    | Minor,      Res ((Unit,_), ()) -> true
    | Major_slice _, Res ((Int,_),r) -> r=0
    | Major,      Res ((Unit,_), ()) -> true
    | Full_major, Res ((Unit,_), ()) -> true
    | Compact,    Res ((Unit,_), ()) -> true
    | Allocated_bytes, Res ((Float,_),r) -> r >= 0.
    | Get_minor_free, Res ((Int,_),r) -> r >= 0
    | Cons64 _,   Res ((Unit,_), ()) -> true
    | AllocStr _, Res ((Unit,_), ()) -> true
    | AllocList _, Res ((Unit,_), ()) -> true
    | RevList _,  Res ((Unit,_), ()) -> true
    | _, _ -> false
end

module GC_STM_seq = STM_sequential.Make(GCConf)
module GC_STM_dom = STM_domain.Make(GCConf)

let _ =
  QCheck_base_runner.run_tests_main [
    GC_STM_seq.agree_test     ~count:1000 ~name:"STM Gc test sequential";
    GC_STM_dom.agree_test_par ~count:1000 ~name:"STM Gc test parallel";
  ]
