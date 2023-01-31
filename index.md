# Multicoretests

### Documentation

This project hosts 4 opam packages:

`multicoretests` - a property-based test suite of the OCaml multicore
runtime system and standard library
 - The test suite [is described in more detail here](https://github.com/ocaml-multicore/multicoretests/blob/main/src/README.md)

`qcheck-lin` - a testing library based on QCheck that generates and
checks parallel tests for sequential consistency
 - [0.1.1](0.1.1/qcheck-lin)
 - [0.1](0.1/qcheck-lin)
 - [development](dev/qcheck-lin)

`qcheck-stm` - a model-based state-machine testing library based on
QCheck that can generate both sequential and parallel tests
 - [0.1.1](0.1.1/qcheck-stm)
 - [0.1](0.1/qcheck-stm)
 - [development](dev/qcheck-stm)

`qcheck-multicoretests-util` - a small library of utility functions
for QCheck-based testing of multicore programs
 - [0.1.1](0.1.1/qcheck-multicoretests-util)
 - [0.1](0.1/qcheck-multicoretests-util)
 - [development](dev/qcheck-multicoretests-util)
