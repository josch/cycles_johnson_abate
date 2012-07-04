all:
	ocamlbuild -classic-display -use-ocamlfind cycles_iter.native cycles_functional.native

clean:
	ocamlbuild -clean
	rm -f *.dot
