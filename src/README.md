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

 - [semaphore/stm_tests.ml](semaphore/stm_tests.ml) contains sequential and
   parallel tests of the `Semaphore.Counting` module

 - [sys/stm_tests.ml](sys/stm_tests.ml) contains sequential and
   parallel tests of the `Sys` module

 - [weak/stm_tests.ml](weak/stm_tests.ml) and
   [weak/stm_tests_hashset.ml](weak/stm_tests_hashset.ml) contains sequential and
   parallel tests of the `Weak` module



Tests utilizing the linearization tests of Lin.ml:

 - [array/lin_internal_tests.ml](array/lin_internal_tests.ml) and [array/lin_tests_dsl.ml](array/lin_tests_dsl.ml)
   contain experimental `Lin.Internal` and `Lin`-tests of `Array`

 - [atomic/lin_internal_tests.ml](atomic/lin_internal_tests.ml) and [atomic/lin_tests_dsl.ml](atomic/lin_tests_dsl.ml)
   contain experimental `Lin.Internal` and `Lin`-tests of `Atomic`

 - [bigarray/lin_tests_dsl.ml](bigarray/lin_tests_dsl.ml) contains experimental `Lin`-tests of `Bigarray`

 - [bytes/lin_tests_dsl.ml](bytes/lin_tests_dsl.ml) contains experimental `Lin`-tests of `Bytes`

 - [dynlink/lin_tests_dsl.ml](dynlink/lin_tests_dsl.ml) contains experimental `Lin`-tests of `Dynlink`

 - [floatarray/lin_tests_dsl.ml](floatarray/lin_tests_dsl.ml) contains experimental `Lin`-tests of `Float.Array`

 - [hashtbl/lin_internal_tests.ml](hashtbl/lin_internal_tests.ml) and [hashtbl/lin_tests_dsl.ml](hashtbl/lin_tests_dsl.ml)
   contain experimental `Lin.Internal` and `Lin`-tests of `Hashtbl`

 - [lazy/lin_internal_tests.ml](lazy/lin_internal_tests.ml) and [lazy/lin_tests_dsl.ml](lazy/lin_tests_dsl.ml)
   contain experimental `Lin.Internal` and `Lin`-tests of `Lazy`

 - [queue/lin_internal_tests.ml](queue/lin_internal_tests.ml) and [queue/lin_tests_dsl.ml](queue/lin_tests_dsl.ml)
   contain experimental `Lin.Internal` and `Lin`-tests of `Queue`

 - [stack/lin_internal_tests.ml](stack/lin_internal_tests.ml) and [stack/lin_tests_dsl.ml](stack/lin_tests_dsl.ml)
   contain experimental `Lin.Internal` and `Lin`-tests of `Stack`



Tests of the underlying spawn/async functionality of `Domain` and
`Thread` (not using `STM.ml` or `Lin.ml` which rely on them):

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

For `Lin.Internal` and `Lin`
- [neg_tests/lin_internal_tests_common.ml](neg_tests/lin_internal_tests_common.ml) and
- [neg_tests/lin_tests_dsl_common.ml](neg_tests/lin_tests_dsl_common.ml)

contain "sanity check tests" for an unprotected global `ref` and a
buggy concurrent list over unboxed `int` and boxed `int64` types.

For `STM`
 - [neg_tests/stm_tests_spec_ref.ml](neg_tests/stm_tests_spec_ref.ml) and
 - [neg_tests/stm_tests_conclist.ml](neg_tests/stm_tests_conclist.ml)

contain similar tests.
