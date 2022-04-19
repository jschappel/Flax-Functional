(lang dune 2.8)
(name flax)

(generate_opam_files true)

(source 
 (github jschappel/Flax-Functional))

(package 
 (name flax)
 (synopsis "A interactive repl for the compiler")
 (depends
  core))

(package 
 (name lib)
 (synopsis "Core functionality for the interpreter")
 (depends
  (ounit2 :with-test)
  core
  ppx_deriving))


