module Refs = struct
  type t = int ref * int ref

  let set_fst (fst, _) v = fst := v
  let get_fst (fst, _) = !fst
  let set_snd (_, snd) v = snd := v
  let get_snd (_, snd) = !snd

  let init () = ref 0, ref 0

  let cleanup _ = ()

  open Lin_base

  let api =
    [ val_ "set_fst" set_fst (t @-> nat_small @-> returning unit)
    ; val_ "set_snd" set_snd (t @-> nat_small @-> returning unit)
    ; val_ "get_fst" get_fst (t @-> returning nat_small)
    ; val_ "get_snd" get_snd (t @-> returning nat_small)
    ]
end

module Mutables = struct
  type t = { mutable fst : int ; mutable snd : int }

  let set_fst t v = t.fst <- v
  let set_snd t v = t.snd <- v
  let get_fst t = t.fst
  let get_snd t = t.snd

  let init () = { fst = 0 ; snd = 0 }

  let cleanup _ = ()

  open Lin_base

  let api =
    [ val_ "set_fst" set_fst (t @-> nat_small @-> returning unit)
    ; val_ "set_snd" set_snd (t @-> nat_small @-> returning unit)
    ; val_ "get_fst" get_fst (t @-> returning nat_small)
    ; val_ "get_snd" get_snd (t @-> returning nat_small)
    ]
end

module Array = struct
  type t = int array

  let len = 2

  let init () = Array.make len 0

  let cleanup _ = ()

  open Lin_base

  let index = int_bound (len - 1)

  let api =
    [ val_ "set" Array.set (t @-> index @-> nat_small @-> returning unit)
    ; val_ "get" Array.get (t @-> index @-> returning nat_small)
    ]
end

module Test_refs = Lin_domain.Make(Refs)
module Test_mutable = Lin_domain.Make(Mutables)
module Test_array = Lin_domain.Make(Array)

let count = 100_000

let () =
  QCheck_base_runner.run_tests_main
   [ Test_refs.neg_lin_test ~count ~name:"Data race: Refs"
   ; Test_mutable.neg_lin_test ~count ~name:"Data race: Mutables"
   ; Test_array.neg_lin_test ~count ~name:"Data race: Array"
   ]
