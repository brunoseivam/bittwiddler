.PHONY: all test clean

test: all testall.sh
	./testall.sh

all: bittwiddler.native

bittwiddler.native:
	opam config exec -- \
	ocamlbuild -use-ocamlfind bittwiddler.native

clean:
	ocamlbuild -clean
	-rm -f *diff *.ll testall.log
