(lang dune 2.9)
(generate_opam_files true)

(license BSD-2-clause)
(maintainers "Jan Midtgaard <mail@janmidtgaard.dk>")
(authors "Jan Midtgaard <mail@janmidtgaard.dk>" )
(source (github jmid/multicoretests))
(documentation "https://github.com/jmid/multicoretests")

(package
 (name multicoretests)
 (synopsis "Experimental multicore tests")
 (depends
  (qcheck        (>= 0.8))
  (ppx_deriving  (>= 5.2))
  (domainslib    (>= 0.3.2)))
)
