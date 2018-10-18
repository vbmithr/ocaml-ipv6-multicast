default: install

install:
	dune build @install

test:
	dune runtest

clean:
	dune clean

.PHONY: all clean install test
