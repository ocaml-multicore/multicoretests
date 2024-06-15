module Dynarray_api = struct
  type t = int Dynarray.t

  let init () = Dynarray.make 1024 0xdeadbeef

  let cleanup _ = ()

  type elem = int [@@warning "-34"]

  let elem = Lin.int

  let get_check a i =
    let v = Dynarray.get a i in
    if not (Obj.is_int (Obj.repr v)) then (Printf.eprintf "dummy found!\n%!"; exit 1) else v

  let api =
    let open Lin in
    let int_not_too_big = int_bound 2048 in
    [ val_ "get_check" get_check (t @-> nat_small @-> returning_or_exc elem);
      val_ "set" Dynarray.set (t @-> nat_small @-> elem @-> returning_or_exc unit);
      val_ "length" Dynarray.length (t @-> returning int);
      val_freq 3 "add_last" Dynarray.add_last (t @-> elem @-> returning_or_exc unit);
      val_ "append_seq" Dynarray.append_seq (t @-> seq elem @-> returning_or_exc unit);
      val_ "get_last" Dynarray.get_last (t @-> returning_or_exc elem);
      val_ "pop_last" Dynarray.pop_last (t @-> returning_or_exc elem);
      val_freq 2 "remove_last" Dynarray.remove_last (t @-> returning_or_exc unit);
      val_ "clear" Dynarray.clear (t @-> returning_or_exc unit);
      val_ "truncate" Dynarray.truncate (t @-> nat_small @-> returning_or_exc unit);
      val_ "ensure_capacity" Dynarray.ensure_capacity (t @-> nat_small @-> returning_or_exc unit);
      val_ "fit_capacity" Dynarray.fit_capacity (t @-> returning_or_exc unit);
      val_ "blit" Dynarray.blit (t @-> int_not_too_big @-> t @-> int_not_too_big @-> int_not_too_big @-> returning_or_exc unit);
      val_freq 2 "set_capacity" Dynarray.set_capacity (t @-> nat_small @-> returning_or_exc unit);
      val_ "reset" Dynarray.reset (t @-> returning_or_exc unit);
      val_freq 3
        "fake"
        (fun a i v ->
          let obj = Obj.repr a in
          let length = (Obj.obj (Obj.field obj 0) : int) in
          let arr = Obj.field obj 1 in
          let arr = (Obj.obj arr : int array) in
          if length <= Array.length arr && i < length then
            Array.unsafe_set arr i v
          else
            ())
        (t @-> nat_small @-> elem @-> returning unit);
    ]
end

module DAT = Lin_domain.Make (Dynarray_api)

let () =
  QCheck_base_runner.run_tests_main
    [ DAT.stress_test ~count:10_000 ~name:"Dynarray does not crash despite races";
    ]
