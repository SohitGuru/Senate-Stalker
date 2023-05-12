.PHONY: test 

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

gui:
	OCAMLRUNPARAM=b dune exec front/gui.exe

repl:
	OCAMLRUNPARAM=b dune exec bin/main.exe

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

zip:
	rm -f keaton.zip
	zip -r keaton.zip . -x@exclude.lst

clean:
	dune clean
	rm -f keaton.zip

doc:
	dune build @doc
