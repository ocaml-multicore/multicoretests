module Dynarray_api = struct
  type t = int Dynarray.t
  let init () = Dynarray.make 1024 0xcafe
  let cleanup _ = ()

  open Lin
  type elem = int [@@warning "-34"]
  let elem = nat_small
  let int = nat_small

  let get_check a i =
    try
      let v = Dynarray.get a i in
      if not (Obj.is_int (Obj.repr v)) then failwith "dummy found!"
    with _ -> ()

  let api =
    (*let int_not_too_big = int_bound 2048 in*)
    [ val_ "get_check" get_check (t @-> int @-> returning unit);
      val_ "set" Dynarray.set (t @-> int @-> elem @-> returning_or_exc unit);
      val_ "length" Dynarray.length (t @-> returning int);
      val_freq 3 "add_last" Dynarray.add_last (t @-> elem @-> returning_or_exc unit);
      val_ "append_seq" Dynarray.append_seq (t @-> seq_small elem @-> returning_or_exc unit);
      val_ "get_last" Dynarray.get_last (t @-> returning_or_exc elem);
      val_ "pop_last" Dynarray.pop_last (t @-> returning_or_exc elem);
      val_freq 2 "remove_last" Dynarray.remove_last (t @-> returning_or_exc unit);
      val_ "clear" Dynarray.clear (t @-> returning_or_exc unit);
      val_ "truncate" Dynarray.truncate (t @-> int @-> returning_or_exc unit);
      val_ "ensure_capacity" Dynarray.ensure_capacity (t @-> int @-> returning_or_exc unit);
      val_ "fit_capacity" Dynarray.fit_capacity (t @-> returning_or_exc unit);
      (*val_ "blit" Dynarray.blit (t @-> int_not_too_big @-> t @-> int_not_too_big @-> int_not_too_big @-> returning_or_exc unit);*)
      val_freq 2 "set_capacity" Dynarray.set_capacity (t @-> int @-> returning_or_exc unit);
      val_ "reset" Dynarray.reset (t @-> returning_or_exc unit);
    ]
end

module DAT = Lin_domain.Make (Dynarray_api)

let () =
  QCheck_base_runner.run_tests_main
    [ DAT.neg_lin_test ~count:1000 ~name:"Lin Dynarray test with Domain";
      (*DAT.stress_test  ~count:100_000 ~name:"Lin Dynarray stress test with Domain";*)
    ]
