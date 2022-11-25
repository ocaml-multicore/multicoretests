# Multicoretests

This project hosts 4 opam packages:

- `multicoretests` - a property-based test suite of the OCaml
   multicore run time system and standard library
- `qcheck-lin` - a testing library based QCheck that generates
   and checks parallel tests for sequential consistency
- `qcheck-stm` - a model-based state-machine testing library based on
   QCheck that can generate both sequential and parallel tests
- `qcheck-multicoretests-util` - a small library of utility functions
   for QCheck-based testing of￼multicore programs

## Documentation

### Development version based on `main`

- [`qcheck-lin`](dev/qcheck-lin)
- [`qcheck-stm`](dev/qcheck-stm)
- [`qcheck-multicoretests-util`](dev/qcheck-multicoretests-util)

The `multicoretests` test suite [is described in more detail here](https://github.com/jmid/multicoretests/blob/main/src/README.md)
