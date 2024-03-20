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

 - [domain/stm_tests_dls.ml](domain/stm_tests_dls.ml) contains sequential and
   parallel tests of the `Domain.DLS` module

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



Tests utilizing `Lin`:

 - [array/lin_internal_tests.ml](array/lin_internal_tests.ml) and [array/lin_tests.ml](array/lin_tests.ml)
   contain experimental `Lin.Internal` and `Lin`-tests of `Array`

 - [atomic/lin_internal_tests.ml](atomic/lin_internal_tests.ml) and [atomic/lin_tests.ml](atomic/lin_tests.ml)
   contain experimental `Lin.Internal` and `Lin`-tests of `Atomic`

 - [bigarray/lin_tests.ml](bigarray/lin_tests.ml) contains experimental `Lin`-tests of `Bigarray`

 - [bytes/lin_tests.ml](bytes/lin_tests.ml) contains experimental `Lin`-tests of `Bytes`

 - [domain/lin_tests_dls.ml](domain/lin_tests_dls.ml) contains experimental `Lin`-tests of `Domain.DLS`

 - [dynlink/lin_tests.ml](dynlink/lin_tests.ml) contains experimental `Lin`-tests of `Dynlink`

 - [ephemeron/lin_tests.ml](ephemeron/lin_tests.ml) contains experimental `Lin`-stress tests of `Ephemeron`

 - [floatarray/lin_tests.ml](floatarray/lin_tests.ml) contains experimental `Lin`-tests of `Float.Array`

 - [hashtbl/lin_internal_tests.ml](hashtbl/lin_internal_tests.ml) and [hashtbl/lin_tests.ml](hashtbl/lin_tests.ml)
   contain experimental `Lin.Internal` and `Lin`-tests of `Hashtbl`

 - [io/lin_internal_tests.ml](hashtbl/lin_internal_tests.ml),
   [io/lin_tests_domain.ml](io/lin_tests_domain.ml), and
   [io/lin_tests_thread.ml](io/lin_tests_thread.ml) contain experimental
   `Lin.Internal` and `Lin`-tests of `In_channel` and `Out_channel`

 - [lazy/lin_internal_tests.ml](lazy/lin_internal_tests.ml) and [lazy/lin_tests.ml](lazy/lin_tests.ml)
   contain experimental `Lin.Internal` and `Lin`-tests of `Lazy`

 - [queue/lin_internal_tests.ml](queue/lin_internal_tests.ml) and [queue/lin_tests.ml](queue/lin_tests.ml)
   contain experimental `Lin.Internal` and `Lin`-tests of `Queue`

 - [stack/lin_internal_tests.ml](stack/lin_internal_tests.ml) and [stack/lin_tests.ml](stack/lin_tests.ml)
   contain experimental `Lin.Internal` and `Lin`-tests of `Stack`

 - [weak/lin_tests.ml](weak/lin_tests.ml) and
   [weak/lin_tests_hashset.ml](weak/lin_tests_hashset.ml) contains experimental
   `Lin`-stress tests of the `Weak` module



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
- [neg_tests/lin_tests_common.ml](neg_tests/lin_tests_common.ml)

contain "sanity check tests" for an unprotected global `ref` and a
buggy concurrent list over unboxed `int` and boxed `int64` types.

For `STM`
 - [neg_tests/stm_tests_spec_ref.ml](neg_tests/stm_tests_spec_ref.ml) and
 - [neg_tests/stm_tests_conclist.ml](neg_tests/stm_tests_conclist.ml)

contain similar tests.
