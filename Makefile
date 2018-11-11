.PHONY: all test clean
all: bittwiddler.native

bittwiddler.native:
	opam config exec -- \
	ocamlbuild -use-ocamlfind bittwiddler.native

clean:
	ocamlbuild -clean
