Statistical tests for development
=================================

We try to apply statistics to ensure that potential improvements are
indeed improvements. This is harder with non-deterministic behaviour.

- To time two versions against eachother
  [hyperfine](https://github.com/sharkdp/hyperfine) is a nice tool. It
  takes care of performing repetitions and making a statistical test.
- For other kinds of numeric data PHK's
  [`ministat`](http://web.mit.edu/freebsd/head/usr.bin/ministat/) is
  handy ([a GitHub copy](https://github.com/cemeyer/ministat-linux)). `ministat` accepts
  two files of numbers and computes a student T-test.
- To compare error rates (number of defects in a production line - or
  falsified QuickCheck properties!) a statistical z-test is handy.
  The below walks through an example using the included `z_test.ml`.


Statistical tests for thread interpretation
-------------------------------------------

This contains a script for performing a statistical test comparing
binomial distributions (these have a yes/no answer).

Use it by passing 4 command-line parameters:
```
  $ z_test.exe 10000 51 10000 44

  z-test of two proportions
  z = 0.719897
  Is |z| = |0.719897| > z_alpha2 = 1.960000 ?
  No, failed to reject null hypothesis
```

Here we give it the count of 10000 trials, with 51 and 44 `yes` outcomes, respectively.
The script will then perform a Z-test deciding whether one distribution is
statistically significant from the other with 95% confidence.


References:

  https://stats.stackexchange.com/questions/113602/test-if-two-binomial-distributions-are-statistically-different-from-each-other
  https://en.wikipedia.org/wiki/Test_statistic
  https://www.itl.nist.gov/div898/handbook/prc/section3/prc33.htm


An example in detail
--------------------

For example, here is a driver running 10.000 tests and recording how
many of them falsifies the tested property. In this case it records
how many test inputs falsified the property `int64 ref`s over
`Thread`s are sequentially consistent:

```ocaml
open QCheck
open Lin_tests_common
open RT_int64
open Util

(** This is a statistics driver of the negative tests over the Thread module *)

let count = ref 0
let t =
  let rep_count = 100 in
  let seq_len,par_len = 20,15 in
  Test.make ~count:10_000 ~retries:5 ~name:("Linearizable ref int64 test with Thread")
    (set_shrink Shrink.nil (arb_cmds_par seq_len par_len))
    (fun triple ->
       try
         let res = repeat rep_count lin_prop_thread triple in
         if not res then count := 1 + !count;
         true
       with
         _ -> count := 1 + !count; true)
;;
QCheck_runner.run_tests ~verbose:true ([t])
;;
Printf.printf "%i / 10_000\n%!" (!count)
```


As an example, we want to compare `interp_thread` with a `Thread.yield`:
```ocaml
  let interp_thread sut cs =
    let cs_arr  = Array.of_list cs in
    let res_arr = Array.map (fun c -> Thread.yield (); Spec.run c sut) cs_arr in
    List.combine cs (Array.to_list res_arr)
```
which produces the following:
```
  random seed: 57832003
  generated error  fail  pass / total     time test name
  [✓] 10000     0     0 10000 / 10000   586.0s Linearizable ref int64 test with Thread
  ================================================================================
  success (ran 1 tests)
  51 / 10_000
```

with an alternative `interp_thread` implementation using `Thread.delay 2e-9`:
```
  let interp_thread sut cs =
    let cs_arr  = Array.of_list cs in
    let res_arr = Array.map (fun c -> Thread.delay 2e-9; Spec.run c sut) cs_arr in
    List.combine cs (Array.to_list res_arr)
```
which produces:
```
  random seed: 241340313
  generated error  fail  pass / total     time test name
  [✓] 10000     0     0 10000 / 10000   974.9s Linearizable ref int64 test with Thread
  ================================================================================
  success (ran 1 tests)
  44 / 10_000
```

Writing these as a table:
```
                        trials    succ     perc.
 yield                  10_000     51      0.0051
 delay2e-9              10_000     44      0.0044

```


The script now performs the following calculation:
```
 H_0: p_yield = p_delay
 H_A: p_yield <> p_delay

                 0.0051 - 0.0044
 z =  ------------------------------------------- = 0.719897
        sqrt( p * (1-p) * (1/10000 + 1/10000) )


       10000 * 0.0051  +  10000 * 0.0044     51 + 44
 p =  ----------------------------------- = --------- = 0.00475
                   10000 + 10000              20000
```

To decide whethere the second worse, we compare `|z| = |0.719897| >
z_alpha2 = 1.960000` and conclude that there is not statistical
significant difference (at 95% confidence).



Aspect that have been tested for `Thread`
-----------------------------------------

Significance of ...
- longer delay? no, delay no better/worse than yield
- while-wait? yes, better
- extra yields in Thread.create? no
- array vs list-alloc?
  - explicit pair-alloc, non-tail rec: yes, better
  - tail-rec, with accumulator: no
- avoid yield in seq.prefix interp? no worse, slightly higher
- avoid pref_obs ref? no worse, slightly higher
- significance of repetition with `repeat`?
   100 improves 75 improves 50 improves 25 improves 10 improves 1
- ...
