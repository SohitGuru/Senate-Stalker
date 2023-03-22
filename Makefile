.PHONY: test 

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

run:
	OCAMLRUNPARAM=b dune exec src/main.exe

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

zip:
	rm -f keaton.zip
	zip -r keaton.zip . -x@exclude.lst

clean:
	dune clean
	rm -f keaton.zip
