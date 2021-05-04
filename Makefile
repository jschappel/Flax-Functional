exe: main.exe
	dune exe ./program.exe
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
