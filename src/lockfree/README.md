Tests of the lockfree library
=============================

This contains a test of work-stealing deque - the only module in lockfree.v0.2.0.

- A previous version found an issue with `size` sometimes returning `-1` when invoked in parallel.
  This observation was taken into account for v.0.2.0, which would eliminate `size` and `is_empty` from the interface,
  leaving only `push`, `pop`, and `steal`.

We test the above in two ways:
- a positive test lets an owner domain invoke `push` and `pop` and a parallel stealer domain invoke only `steal`.
- a negative test lets both owner and stealer invoke all three operations, thus breaking the contract.




Older tests
-----------

An older version tested lockfree.0.1.3 in which two linearizability tests would fail:

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


