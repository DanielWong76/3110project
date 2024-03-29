.PHONY: test

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec ./test/main.exe

game:
	OCAMLRUNPARAM=b dune exec ./bin/main.exe
