(* parallel stress tests of the GC with explicit Gc invocations *)


module Spec =
struct
  open QCheck
  open STM

  type cmd =
    | Set_minor_heap_size_2048
    | Compact
    | PreAllocList of int * unit list
    | RevList of int

  let show_cmd x = match x with
    | Set_minor_heap_size_2048 -> "Set minor_heap_size 2048"
    | Compact     -> "Compact"
    | PreAllocList (i,_l) -> "PreAllocList " ^ (string_of_int i)
    | RevList i   -> "RevList " ^ (string_of_int i)

  type state = unit
  let init_state = ()

  let array_length = 4

  let gc_cmds =
    let list_gen = Gen.map (fun l -> List.init l (fun _ -> ())) Gen.nat in
    let index_gen = Gen.int_bound (array_length-1) in
    Gen.([
        5, map2 (fun index list -> PreAllocList (index,list)) index_gen list_gen;
        5, map (fun index -> RevList index) index_gen;
        1, return Set_minor_heap_size_2048;
        1, return Compact;
      ])

  let arb_cmd _s = QCheck.make ~print:show_cmd (Gen.frequency gc_cmds)

  let next_state _n _s = ()

  type sut = unit list array
  let init_sut () = Array.make array_length []

  let orig_control = Gc.get ()

  let cleanup sut =
    begin
      for i=0 to array_length-1 do
        sut.(i) <- [];
      done;
      Gc.set orig_control;
      Gc.major ()
    end

  let precond _n _s = true

  let run c sut = match c with
    | Set_minor_heap_size_2048 -> Res (unit, Gc.set { orig_control with minor_heap_size = 2048; })
    | Compact     -> Res (unit, Gc.compact ())
    | PreAllocList (i,l) -> Res (unit, sut.(i) <- l) (*alloc list in parent domain in test-input*)
    | RevList i -> Res (unit, sut.(i) <- List.rev sut.(i)) (*alloc list at test runtime*)

  let postcond _n (_s: state) _res = false (* unused in stress_test_par mode *)
end

module GC_STM_dom = STM_domain.Make(Spec)

let _ =
  QCheck.Test.check_exn (
    GC_STM_dom.stress_test_par    ~count:2000 ~name:"STM Gc stress test parallel";
  )
