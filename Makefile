exe: main.exe
	$(dune build bin/program.exe)
	dune exec bin/program.exe

main.exe: install

fast_test: install
	dune runtest

test: clean install
	dune runtest

clean:
	dune clean

install:
	dune build

fmt:
	dune build @fmt --auto-promote

init:
	$(shell eval $(opam env))
