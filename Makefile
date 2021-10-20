exe: main.exe
	$(dune build bin/program.exe)
	rlwrap dune exec bin/program.exe
	$(echo "")


main.exe: install

fast_test: install
	dune runtest

test: clean install
	dune runtest

clean:
	dune clean

install:
	dune build
