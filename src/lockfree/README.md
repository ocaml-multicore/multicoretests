Tests of the lockfree library
=============================

Two linearizability tests fail:

- The Lockfree.Hash test in [lin_tests.ml](lin_tests.ml)

  This is believed to by a genuine issue with parallel
  `Lockfree.Hash.add`:

  ```
  Messages for test Linearizable lockfree Hash test:

  Results incompatible with sequential execution

                                     |
                         (Add (5, '\198')) : RAdd
                                     |
                  .------------------------------------.
                  |                                    |
     (Remove 5) : (RRemove true)          (Add (767, '\224')) : RAdd
                                            Elem_of : (RElem_of [])
  ```

  Here the added `767` entry disappears - as the following `elem_of`
  operation returns an empty list of bindings.


- The Lockfree.Bag test in [lin_tests.ml](lin_tests.ml)

  This code is not linearizable, as the return values vary between
  parallel/sequential execution. The test should be replaced by a
  model-based test.


