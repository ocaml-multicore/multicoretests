Current (experimental) PBTs of multicore
========================================

Tests utilizing the parallel STM.ml capability:

 - [atomic/stm_tests.ml](atomic/stm_tests.ml) contains sequential and
   parallel tests of the `Atomic` module using STM.ml

 - [buffer/stm_tests.ml](buffer/stm_tests.ml) contains sequential and
   parallel tests of the `Buffer` module using STM.ml

 - [ephemeron/stm_tests.ml](ephemeron/stm_tests.ml) contains sequential and
   parallel tests of the `Ephemeron` module using STM.ml

 - [lazy/stm_tests.ml](lazy/stm_tests.ml) contains sequential and
   parallel tests of the `Lazy` module using STM.ml

 - [lockfree/ws_deque_test.ml](lockfree/ws_deque_test.ml) contains sequential
   and parallel tests of [ws_deque.ml](https://github.com/ocaml-multicore/lockfree/blob/main/src/ws_deque.ml)
   from [Lockfree](https://github.com/ocaml-multicore/lockfree) using STM.ml

 - [domainslib/chan_stm_tests.ml](domainslib/chan_stm_tests.ml) contains sequential and
   parallel tests of the `Chan` module from [Domainslib](https://github.com/ocaml-multicore/domainslib)
   using STM.ml



Tests utilizing the linearizability tests of Lin.ml:

 - [atomic/lin_tests.ml](atomic/lin_tests.ml) contains experimental `Lin`-tests of `Atomic`

 - [ephemeron/lin_tests_dsl.ml](ephemeron/lin_tests_dsl.ml) contains experimental `Lin_api`-tests of `Ephemeron`

 - [hashtbl/lin_tests.ml](hashtbl/lin_tests.ml) and [hashtbl/lin_tests_dsl.ml](hashtbl/lin_tests_dsl.ml)
   contains experimental `Lin` and `Lin_api`-tests of `Hashtbl`

 - [queue/lin_tests.ml](queue/lin_tests.ml) contains experimental `Lin`-tests of `Queue`

 - [stack/lin_tests.ml](stack/lin_tests.ml) contains experimental `Lin`-tests of `Stack`

 - [lazy/lin_tests.ml](lazy/lin_tests.ml) contains experimental `Lin`-tests of `Lazy`

 - [kcas/lin_tests.ml](kcas/lin_tests.ml) contains experimental
   `Lin`-tests of `Kcas` and `Kcas.W1` (Note: `Kcas` is subsumed by `Stdlib.Atomic`).



Tests of the underlying spawn/async functionality of Domain and
Domainslib.Task (not using STM.ml or Lin.ml which rely on them):

 - [domainslib/task_one_dep.ml](domainslib/task_one_dep.ml) is a test of `Domainslib.Task`'s `async`/`await`.

 - [domainslib/task_more_deps.ml](domainslib/task_more_deps.ml) is a variant of the
   above allowing each promise to await on multiple others.

 - [domainslib/task_parallel.ml](domainslib/task_parallel.ml) test the three `Domainslib.Task.parallel_*` operations.

 - [domain/domain_joingraph.ml](domain/domain_joingraph.ml) is a test of `Domain`'s
   `spawn`/`join` based on a random dependency graph

 - [domain/domain_spawntree.ml](domain/domain_spawntree.ml) is a test of `Domain`'s
   `spawn`/`join` based on a random `spawn` tree

 - [thread/thread_joingraph.ml](thread/thread_joingraph.ml) is a test of `Thread`'s
   `create`/`join` based on a random dependency graph

 - [thread/thread_createtree.ml](thread/thread_createtree.ml) is a test of `Thread`'s
   `create`/`join` based on a random `create` tree


Development tests
-----------------

During development we use examples with known problems to help ensure
that concurrency issues are indeed found as expected (aka. sanity
check).

For `Lin`
- [src/neg_tests/lin_tests_common.ml](src/neg_tests/lin_tests_common.ml) and
- [src/neg_tests/domain_lin_tests.ml](src/neg_tests/domain_lin_tests.ml)

contain "sanity check tests" for an unprotected global `ref` and a
buggy concurrent list over unboxed `int` and boxed `int64` types.

For `STM`
 - [src/neg_tests/ref_stm_tests.ml](src/neg_tests/ref_stm_tests.ml) and
 - [src/neg_tests/conclist_stm_tests.ml](src/neg_tests/conclist_stm_tests.ml)

contain similar tests.
