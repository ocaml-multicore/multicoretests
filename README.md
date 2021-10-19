Multicore tests
===============

Experimental property-based tests of (parts of) the forthcoming OCaml multicore compiler.

 - [src/task_one_dep.ml](src/task_one_dep.ml) is a test of `Domainslib.Task`'s `async`/`await`.

 - [src/task_one_dep.ml](src/task_one_dep.ml) is a variant of the
   above allowing each promise to await on multiple others.

 - [src/atomictest.ml](src/atomictest.ml) is a sequential test of the `Atomic` module using qcstm

 - [src/domain_joingraph.ml](src/domain_joingraph.ml) is a test of `Domain`'s
   `spawn`/`join` based on a random dependency graph

 - [src/domain_spawntree.ml](src/domain_spawntree.ml) is a test of `Domain`'s
   `spawn`/`join` based on a random spawn tree

 - [src/ws_deque_test.ml](src/ws_deque_test.ml) is a sequential test of
   [ws_deque.ml](https://github.com/ocaml-multicore/domainslib/blob/master/lib/ws_deque.ml)
   from [Domainslib](https://github.com/ocaml-multicore/domainslib).
   We include a verbatim copy in [lib/ws_deque.ml](lib/ws_deque.ml) for convenience.


Issues
======

Dead-lock in Domainslib
-----------------------

Tests in
[src/task_one_dep.ml](src/task_one_dep.ml) and
[src/task_more_deps.ml](src/task_more_deps.ml) are run with a timeout
to prevent deadlocking indefinitely.

[src/task_one_dep.ml](src/task_one_dep.ml) can trigger one such deadlock.
It exhibits no non-determistic behaviour when repeating the same
tested property from within the QCheck test.
However it fails (due to timeout) on the following test input:

```ocaml
$ dune exec -- src/task_one_dep.exe -v
random seed: 147821373
generated error fail pass / total     time test name
[✗]   25    0    1   24 /  100    36.2s Task.async/await

--- Failure --------------------------------------------------------------------

Test Task.async/await failed (2 shrink steps):

{ num_domains = 3; length = 6;
  dependencies = [|None; (Some 0); None; (Some 1); None; None|] }
================================================================================
failure (1 tests failed, 0 tests errored, ran 1 tests)
```

This corresponds to the program (available in
[issues/task_issue.ml](issues/task_issue.ml))
with 3+1 domains and 6 promises.
It loops infinitely with both bytecode/native:

```ocaml
...
open Domainslib

(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
           else z

let work () =
  for _ = 1 to 200 do
    assert (7 = tak 18 12 6);
  done

let pool = Task.setup_pool ~num_additional_domains:3 ()

let p0 = Task.async pool work
let p1 = Task.async pool (fun () -> work (); Task.await pool p0)
let p2 = Task.async pool work
let p3 = Task.async pool (fun () -> work (); Task.await pool p1)
let p4 = Task.async pool work
let p5 = Task.async pool work

let () = List.iter (fun p -> Task.await pool p) [p0;p1;p2;p3;p4;p5]
let () = Task.teardown_pool pool
```


Utop segfault
-------------

Utop segfaults when loading [src/domaintest.ml](src/domaintest.ml)
interactively:

``` ocaml
$ utop
──────────────────────────────────────────────┬─────────────────────────────────────────────────────────────────────┬──────────────────────────────────────────────
                                              │ Welcome to utop version 2.8.0 (using OCaml version 4.12.0+domains)! │                                              
                                              └─────────────────────────────────────────────────────────────────────┘                                              
Findlib has been successfully loaded. Additional directives:
  #require "package";;      to load a package
  #list;;                   to list the available packages
  #camlp4o;;                to load camlp4 (standard syntax)
  #camlp4r;;                to load camlp4 (revised syntax)
  #predicates "p,q,...";;   to set these predicates
  Topfind.reset();;         to force that packages will be reloaded
  #thread;;                 to enable threads


Type #utop_help for help about using utop.

utop # #require "ppx_deriving.show";;
utop # #require "qcheck";;
utop # #use "src/domaintest.ml";;
type cmd = Incr | Decr | Spawn of cmd list
val pp_cmd : Format.formatter -> cmd -> unit = <fun>
val show_cmd : cmd -> string = <fun>
val count_spawns : cmd -> int = <fun>
val gen : int -> int -> cmd Gen.t = <fun>
val shrink_cmd : cmd Shrink.t = <fun>
val interp : int -> cmd -> int = <fun>
val dom_interp : int Atomic.t -> cmd -> unit = <fun>
val t : max_depth:int -> max_width:int -> Test.t = <fun>
random seed: 84598291
Segmentation fault (core dumped)
```

This does happen when running a plain `ocaml` top-level though, so it
seems `utop`-specific.
