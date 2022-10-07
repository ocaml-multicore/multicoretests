(* ********************************************************************** *)
(*           Tests of thread-unsafe [Bigarray.Array1] of ints             *)
(* ********************************************************************** *)

(* Bigarray.Array1 API:

    val create : ('a, 'b) Bigarray.kind -> 'c Bigarray.layout -> int -> ('a, 'b, 'c) t
    val init : ('a, 'b) Bigarray.kind -> 'c Bigarray.layout -> int -> (int -> 'a) -> ('a, 'b, 'c) t
    external dim : ('a, 'b, 'c) t -> int
    external kind : ('a, 'b, 'c) t -> ('a, 'b) Bigarray.kind
    external layout : ('a, 'b, 'c) t -> 'c Bigarray.layout
    val change_layout : ('a, 'b, 'c) t -> 'd Bigarray.layout -> ('a, 'b, 'd) t
    val size_in_bytes : ('a, 'b, 'c) t -> int
    external get : ('a, 'b, 'c) t -> int -> 'a
    external set : ('a, 'b, 'c) t -> int -> 'a -> unit
    external sub : ('a, 'b, 'c) t -> int -> int -> ('a, 'b, 'c) t
    val slice : ('a, 'b, 'c) t -> int -> ('a, 'b, 'c) Bigarray.Array0.t
    external blit : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit
    external fill : ('a, 'b, 'c) t -> 'a -> unit
    val of_array : ('a, 'b) Bigarray.kind -> 'c Bigarray.layout -> 'a array -> ('a, 'b, 'c) t
    external unsafe_get : ('a, 'b, 'c) t -> int -> 'a
    external unsafe_set : ('a, 'b, 'c) t -> int -> 'a -> unit
*)

module BA1Conf =
struct
  open Bigarray
  type t = (int, int_elt, c_layout) Array1.t

  let array_create sz =
    let arr = Array1.create int C_layout sz in
    Array1.fill arr 0 ;
    arr
  let of_array = Array1.of_array int C_layout
  let dummy_change_layout arr = Array1.change_layout arr C_layout

  let array_size = 16
  let init () = array_create array_size
  let cleanup _ = ()

  open Lin_api
  let int = int_small

  let api =
    [ val_ "Bigarray.Array1.create"        array_create         (int @-> returning_ t);
      (* [Array1.init] requires a function *)
      val_ "Bigarray.Array1.dim"           Array1.dim           (t @-> returning int);
      (* [Array1.kind] returns an untestable value *)
      (* [change_layout]: the layout is fixed in our sut, so we test a dummy version *)
      val_ "Bigarray.Array1.change_layout" dummy_change_layout  (t @-> returning_ t);
      val_ "Bigarray.Array1.size_in_bytes" Array1.size_in_bytes (t @-> returning int);
      val_ "Bigarray.Array1.get"           Array1.get           (t @-> int @-> returning_or_exc int);
      val_ "Bigarray.Array1.set"           Array1.set           (t @-> int @-> int @-> returning_or_exc unit);
      val_ "Bigarray.Array1.sub"           Array1.sub           (t @-> int @-> int @-> returning_or_exc_ t);
      (* [Array1.slice]: cannot be tested since it produces a Bigarray.Array0.t *)
      val_ "Bigarray.Array1.blit"          Array1.blit          (t @-> t @-> returning unit);
      val_ "Bigarray.Array1.fill"          Array1.fill          (t @-> int @-> returning unit);
      val_ "Bigarray.Array1.of_array"      of_array             (bounded_array 100 int @-> returning_ t);
      (* [Array1.unsafe_get] and [Array1.unsafe_set] cannot be tested:
         they can segfault or produce any useless result *)
    ]
end

module BA1T = Lin_api.Make(BA1Conf)

let _ =
  Util.set_ci_printing () ;
  QCheck_base_runner.run_tests_main [
    BA1T.neg_lin_test `Domain ~count:1000 ~name:"Lin_api Bigarray.Array1 (of ints) test with Domain";
  ]
