.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f cs3110_final_project.zip
	zip -r cs3110_final_project.zip . -x@exclude.lst

clean:
	dune clean
	rm -f cs3110_final_project.zip

doc:
	dune build @doc

docs:
	dune build @doc
