Current (experimental) PBTs of multicore
========================================

Tests utilizing the parallel STM.ml capability:

 - [array/stm_tests.ml](array/stm_tests.ml) contains sequential and
   parallel tests of the `Array` module

 - [atomic/stm_tests.ml](atomic/stm_tests.ml) contains sequential and
   parallel tests of the `Atomic` module

 - [bigarray/stm_tests.ml](bigarray/stm_tests.ml) contains sequential and
   parallel tests of the `Bigarray` module

 - [buffer/stm_tests.ml](buffer/stm_tests.ml) contains sequential and
   parallel tests of the `Buffer` module

 - [bytes/stm_tests.ml](bytes/stm_tests.ml) contains sequential and
   parallel tests of the `Bytes` module

 - [ephemeron/stm_tests.ml](ephemeron/stm_tests.ml) contains sequential and
   parallel tests of the `Ephemeron` module

 - [floatarray/stm_tests.ml](floatarray/stm_tests.ml) contains sequential and
   parallel tests of the `Float.Array` module

 - [hashtbl/stm_tests.ml](hashtbl/stm_tests.ml) contains sequential and
   parallel tests of the `Hashtbl` module

 - [lazy/stm_tests.ml](lazy/stm_tests.ml) contains sequential and
   parallel tests of the `Lazy` module

 - [lockfree/ws_deque_test.ml](lockfree/ws_deque_test.ml) contains sequential
   and parallel tests of [ws_deque.ml](https://github.com/ocaml-multicore/lockfree/blob/main/src/ws_deque.ml)
   from [Lockfree](https://github.com/ocaml-multicore/lockfree)

 - [domainslib/chan_stm_tests.ml](domainslib/chan_stm_tests.ml) contains sequential and
   parallel tests of the `Chan` module from [Domainslib](https://github.com/ocaml-multicore/domainslib)




Tests utilizing the linearizability tests of Lin.ml:

 - [array/lin_tests.ml](array/lin_tests.ml) and [array/lin_tests_dsl.ml](array/lin_tests_dsl.ml)
   contain experimental `Lin` and `Lin_api`-tests of `Array`

 - [atomic/lin_tests.ml](atomic/lin_tests.ml) and [atomic/lin_tests_dsl.ml](atomic/lin_tests_dsl.ml)
   contain experimental `Lin` and `Lin_api`-tests of `Atomic`

 - [bigarray/lin_tests_dsl.ml](bigarray/lin_tests_dsl.ml) contains experimental `Lin_api`-tests of `Bigarray`

 - [bytes/lin_tests_dsl.ml](bytes/lin_tests_dsl.ml) contains experimental `Lin_api`-tests of `Bytes`

 - [ephemeron/lin_tests_dsl.ml](ephemeron/lin_tests_dsl.ml) contains experimental `Lin_api`-tests of `Ephemeron`

 - [floatarray/lin_tests_dsl.ml](floatarray/lin_tests_dsl.ml) contains experimental `Lin_api`-tests of `Float.Array`

 - [hashtbl/lin_tests.ml](hashtbl/lin_tests.ml) and [hashtbl/lin_tests_dsl.ml](hashtbl/lin_tests_dsl.ml)
   contain experimental `Lin` and `Lin_api`-tests of `Hashtbl`

 - [lazy/lin_tests.ml](lazy/lin_tests.ml) and [lazy/lin_tests_dsl.ml](lazy/lin_tests_dsl.ml)
   contain experimental `Lin` and `Lin_api`-tests of `Lazy`

 - [queue/lin_tests.ml](queue/lin_tests.ml) and [queue/lin_tests_dsl.ml](queue/lin_tests_dsl.ml)
   contain experimental `Lin` and `Lin_api`-tests of `Queue`

 - [stack/lin_tests.ml](stack/lin_tests.ml) and [stack/lin_tests_dsl.ml](stack/lin_tests_dsl.ml)
   contain experimental `Lin` and `Lin_api`-tests of `Stack`

 - [kcas/lin_tests.ml](kcas/lin_tests.ml) and [kcas/lin_tests_dsl.ml](kcas/lin_tests_dsl.ml)
   contain experimental `Lin` and `Lin_api`-tests of `Kcas` and `Kcas.W1` (Note: `Kcas` is subsumed by `Stdlib.Atomic`).



Tests of the underlying spawn/async functionality of `Domain`,
`Domainslib.Task`, and `Thread` (not using `STM.ml` or `Lin.ml` which rely on them):

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
- [neg_tests/lin_tests_common.ml](neg_tests/lin_tests_common.ml) and
- [neg_tests/domain_lin_tests.ml](neg_tests/domain_lin_tests.ml)

contain "sanity check tests" for an unprotected global `ref` and a
buggy concurrent list over unboxed `int` and boxed `int64` types.

For `STM`
 - [neg_tests/ref_stm_tests.ml](neg_tests/ref_stm_tests.ml) and
 - [neg_tests/conclist_stm_tests.ml](neg_tests/conclist_stm_tests.ml)

contain similar tests.
