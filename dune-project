(lang dune 2.8)
(name flax)

(generate_opam_files true)

(using menhir 2.1)

(source 
 (github jschappel/Flax-Functional))

(package 
 (name flax_repl)
 (synopsis "A interactive repl for the compiler")
 (depends
  core))

(package 
 (name flax_parser)
 (synopsis "A interactive repl for the compiler")
 (depends
  core
  ppx_deriving))

(package 
 (name flax_core)
 (synopsis "Core functionality for the compiler")
 (depends
  (ounit2 :with-test)
  core
  ppx_deriving))


