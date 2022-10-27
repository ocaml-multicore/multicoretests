include Stdlib

module Atomic =
struct
  include Stdlib.Atomic
  let get a = Thread.yield (); get a
  let set a v = Thread.yield (); set a v
  let exchange a v = Thread.yield (); exchange a v
  let compare_and_set a v w = Thread.yield (); compare_and_set a v w
  let fetch_and_add a v = Thread.yield (); fetch_and_add a v
  let incr a = Thread.yield (); incr a
  let decr a = Thread.yield (); decr a
end

(* Attempts to trigger issues *)
let ignore a = Thread.yield (); Stdlib.(ignore a)

let (!) rcell = Thread.yield (); Stdlib.(! rcell)
let (:=) lval rval = Thread.yield (); Stdlib.(lval := rval)
let incr rcell = Thread.yield (); Stdlib.incr rcell
let decr rcell = Thread.yield (); Stdlib.decr rcell

