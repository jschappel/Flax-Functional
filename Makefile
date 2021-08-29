exe: main.exe
	$(dune build bin/program.exe)
	dune exec bin/program.exe
	$(echo "")


main.exe: install

fast_test:
	dune runtest

test: clean install
	dune runtest

clean:
	dune clean

install:
	dune build
