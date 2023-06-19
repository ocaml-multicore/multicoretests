# Changes

## Next version

- #324: When a command causes an unexpectedly uncaught exception,
        display that command along with the exception itself in both
        `Lin` and `STM`

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
