open QCheck
open STM

(* sequential and parallel tests of the GC *)

module GCConf =
struct
  type cmd =
    | Counters
    | Minor
    | Full_major
    | Compact
    | Cons64 of int

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Counters   -> cst0 "Counters" fmt
    | Minor      -> cst0 "Minor" fmt
    | Full_major -> cst0 "Full_major" fmt
    | Compact    -> cst0 "Compact" fmt
    | Cons64 i   -> cst1 pp_int "Cons64" par fmt i

  let show_cmd = Util.Pp.to_show pp_cmd

  type state = unit
  type sut   = { mutable int64s : int64 list }

  let arb_cmd _s =
    let int_gen = Gen.small_nat in
    QCheck.make ~print:show_cmd
      Gen.(frequency
             [ 1, return Counters;
               1, return Minor;
               1, return Full_major;
               1, return Compact;
               20, map (fun i -> Cons64 i) int_gen;
             ])

  let init_state  = ()

  let next_state n _s = match n with
    | Counters   -> ()
    | Minor      -> ()
    | Full_major -> ()
    | Compact    -> ()
    | Cons64 _   -> ()

  let init_sut () = { int64s = [] }

  let cleanup sut =
    begin
      sut.int64s <- [];
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
    | Counters   -> Res (tup3 float float float, Gc.counters ())
    | Minor      -> Res (unit, Gc.minor ())
    | Full_major -> Res (unit, Gc.full_major ())
    | Compact    -> Res (unit, Gc.compact ())
    | Cons64 i   -> Res (unit, sut.int64s <- (Int64.of_int i)::sut.int64s) (*alloc int64 and cons cell at test runtime*)

  let postcond n (_s: unit) res = match n, res with
    | Counters, Res ((Tup3 (Float,Float,Float),_),r) ->
      let (minor_words, promoted_words, major_words) = r in
      minor_words >= 0. && promoted_words >= 0. && major_words >= 0.
    | Minor,      Res ((Unit,_), ()) -> true
    | Full_major, Res ((Unit,_), ()) -> true
    | Compact,    Res ((Unit,_), ()) -> true
    | Cons64 _,   Res ((Unit,_), ()) -> true
    | _, _ -> false
end


module GC_STM_seq = STM_sequential.Make(GCConf)
module GC_STM_dom = STM_domain.Make(GCConf)

let _ =
  QCheck_base_runner.run_tests_main [
    GC_STM_seq.agree_test      ~count:1000 ~name:"STM Gc test sequential";
    GC_STM_dom.agree_test_par  ~count:1000 ~name:"STM Gc test parallel";
    GC_STM_dom.stress_test_par ~count:1000 ~name:"STM Gc stress test parallel";
  ]
