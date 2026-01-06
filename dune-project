(lang dune 3.18)

(name multicoretests)

(generate_opam_files true)

(source
 (github ocaml-multicore/multicoretests))

(authors "Jan Midtgaard" "Olivier Nicole" "Nicolas Osborne" "Samuel Hym")

(maintainers "Jan Midtgaard <mail@janmidtgaard.dk>")

(license BSD-2-clause)

(version "0.11")

(maintenance_intent "(latest)")

(package
 (name multicoretests)
 (synopsis "Experimental multicore test suite of OCaml 5.0")
 (authors "Multiple contributors")
 (description
  "This package contains a collection of randomized QCheck tests to exercise\nthe multicore run-time of OCaml 5.0.")
 (tags
  ("test"
   "test suite"
   "property"
   "qcheck"
   "quickcheck"
   "multicore"
   "non-determinism"))
 (depends
  base-domains
  (qcheck-core
   (>= "0.90"))
  (qcheck-lin
   (= :version))
  (qcheck-stm
   (= :version))))

(package
 (name qcheck-stm)
 (synopsis
  "State-machine testing library for sequential and parallel model-based tests")
 (description
  "A state-machine testing library based on QCheck that can generate both\nsequential and parallel tests against a declarative model.")
 (tags
  ("test"
   "property"
   "qcheck"
   "quickcheck"
   "state-machine testing"
   "model-based testing"
   "parallel testing"))
 (depopts base-domains)
 (depends
  (ocaml
   (>= 4.12))
  (qcheck-core
   (>= "0.90"))
  (eio
   (>= "1.0"))
  (qcheck-multicoretests-util
   (= :version))))

(package
 (name qcheck-lin)
 (synopsis "A multicore testing library for OCaml")
 (description
  "A testing library based on QCheck to test interface behaviour under parallel\nusage. Lin will generate and run random parallel tests and check the observed\nbehaviour for sequential consistency, that is, whether they can be linearized\nand explained by some sequential interleaving.")
 (tags
  ("test"
   "property"
   "qcheck"
   "quickcheck"
   "parallelism"
   "sequential consistency"))
 (depopts base-domains)
 (depends
  (ocaml
   (>= 4.12))
  (qcheck-core
   (>= "0.90"))
  (qcheck-multicoretests-util
   (= :version))))

(package
 (name qcheck-multicoretests-util)
 (synopsis
  "Various utility functions for property-based testing of multicore programs")
 (description
  "A small library of utility functions for QCheck-based testing of\nmulticore programs.")
 (tags
  ("test" "property" "qcheck" "quickcheck" "multicore" "non-determinism"))
 (depends
  (ocaml
   (>= 4.12))
  (qcheck-core
   (>= "0.90"))))
