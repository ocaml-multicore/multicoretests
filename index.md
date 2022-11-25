# Multicoretests

## Documentation

This project hosts 4 opam packages:

- `multicoretests` - a property-based test suite of the OCaml
   multicore runtime system and standard library
  - The test suite [is described in more detail here](https://github.com/jmid/multicoretests/blob/main/src/README.md)
- `qcheck-lin` - a testing library based on QCheck that generates
   and checks parallel tests for sequential consistency
  - [development](dev/qcheck-lin)
  - [0.1](dev/qcheck-lin)
- `qcheck-stm` - a model-based state-machine testing library based on
   QCheck that can generate both sequential and parallel tests
  - [development](dev/qcheck-stm)
  - [0.1](dev/qcheck-stm)
- `qcheck-multicoretests-util` - a small library of utility functions
   for QCheck-based testing of multicore programs
  - [development](dev/qcheck-multicoretests-util)
  - [0.1](dev/qcheck-multicoretests-util)
