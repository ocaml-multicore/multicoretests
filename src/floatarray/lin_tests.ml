(* ********************************************************************** *)
(*                 Tests of thread-unsafe [Float.Array]                   *)
(* ********************************************************************** *)

module FAConf =
struct
  type t = Float.Array.t

  let array_size = 16
  let init () = Float.Array.make array_size 0.0
  let cleanup _ = ()

  open Lin
  let int = int_small

  (* fully evaluate the iterator, otherwise we get too many
     counterexamples from this aspect *)
  let strict_to_seq a = List.to_seq (List.of_seq (Float.Array.to_seq a))

  let api =
    [ val_ "Float.Array.length"      Float.Array.length                      (t@-> returning int);
      val_ "Float.Array.get"         Float.Array.get                         (t@-> int @-> returning_or_exc float);
      val_ "Float.Array.set"         Float.Array.set                         (t@-> int @-> float @-> returning_or_exc unit);
      val_ "Float.Array.fill"        Float.Array.fill                        (t@-> int @-> int @-> float @-> returning_or_exc unit);
      val_ "Float.Array.to_list"     Float.Array.to_list                     (t@-> returning (list float));
      val_ "Float.Array.mem"         Float.Array.mem                         (float @-> t @-> returning bool);
      val_ "Float.Array.mem_ieee"    Float.Array.mem_ieee                    (float @-> t @-> returning bool);
      val_ "Float.Array.sort"        (Float.Array.sort compare)              (t@-> returning unit);
      val_ "Float.Array.stable_sort" (Float.Array.stable_sort compare)       (t@-> returning unit);
      val_ "Float.Array.fast_sort"   (Float.Array.fast_sort compare)         (t@-> returning unit);
      val_ "Float.Array.to_seq"      strict_to_seq                           (t@-> returning (seq float));
      val_ "Float.Array.to_array"    (Float.Array.map_to_array (fun x -> x)) (t@-> returning (array float));
    ]
end

module FAT = Lin_domain.Make(FAConf)

let _ =
  QCheck_base_runner.run_tests_main [
    FAT.neg_lin_test ~count:1000 ~name:"Lin Float.Array test with Domain";
    FAT.stress_test  ~count:1000 ~name:"Lin Float.Array stress test with Domain";
  ]
