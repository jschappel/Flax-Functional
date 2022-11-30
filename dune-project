(lang dune 2.8)
(name flax)

(generate_opam_files true)

(using menhir 2.1)

(source 
 (github jschappel/Flax-Functional))
 (package 
 (name flax)
 (authors "Joshua Schappel")
 (maintainers "Joshua Schappel")
 (synopsis "Core functionality for the compiler")
 (depends
  (ounit2 :with-test)
  base
  menhir
  ppx_jane
  ppx_deriving))

