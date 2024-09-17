# Changes

## 0.4

- #415: Remove `--verbose` in internal `mutable_set_v5` expect test to avoid
  a test failure on a slow machine
- #443: Add `Lin_domain.stress_test` as a lighter stress test, not
  requiring an interleaving search.
- #462: Add `STM_domain.stress_test_par`, similar to `Lin_domain.stress_test`
  for STM models.
- #472: Switch `arb_cmds` to use an exponential distribution with a
  mean of 10, avoiding lists of up to 10000 cmds in `STM_sequential`
  (reported by @nikolaushuber).

## 0.3

- #400: Catch and delay exceptions in `STM`'s `next_state` for a nicer UX
- #387: Reduce needless allocations in `Lin`'s sequential consistency
  search, as part of an `Out_channel` test cleanup
- #379: Extend the set of `Util.Pp` pretty-printers and teach them to
  add break hints similar to `ppx_deriving.show`; teach `to_show` to
  generate truncated strings when `$MCTUTILS_TRUNCATE` environment
  variable is set
- #368: Switch `STM_domain.agree_prop_par_asym` from using
  `Semaphore.Binary` to using an `int Atomic.t` which improves
  the error rate across platforms and backends

## 0.2

- #342: Add two submodules of combinators in `Util`:
  - `Pp` to pretty-print values back to valid OCaml syntax
  - `Equal` to test equality of values
- #337: Add 3 `Bytes.t` combinators to `Lin`: `bytes`, `bytes_small`, `bytes_small_printable`
- #329,340,352: Support `qcheck-lin` and `qcheck-stm` on OCaml 4.12.x, 4.13.x and 4.14.x
                without the `Domain` and `Effect` modes
- #316: Fix `rep_count` in `STM_thread` so that negative and positive
  tests repeat equally many times
- #318: avoid repetitive interleaving searches in `STM_domain` and `STM_thread`
- #312: Escape and quote `bytes` printed with `STM`'s `bytes` combinator
- #295: ensure `cleanup` is run in the presence of exceptions in
  - `STM_sequential.agree_prop` and `STM_domain.agree_prop_par`
  - `Lin_thread.lin_prop` and `Lin_effect.lin_prop`

## 0.1.1

- #263: Cleanup resources after each domain-based `Lin` test
- #281: Escape and quote strings printed with `STM`'s `string` combinator

## 0.1

The initial opam release of `qcheck-lin`, `qcheck-stm`, and
`qcheck-multicoretests-util`.

The `multicoretests` package is not released on opam, as it is of
limited use to OCaml developers.
