.PHONY: all test clean

all: bittwiddler.native runtime.o

test: all testall.sh
	./testall.sh

bittwiddler.native:
	opam config exec -- \
	ocamlbuild -use-ocamlfind bittwiddler.native

clean:
	ocamlbuild -clean
	-rm -f *diff *.ll *.err *.s testall.log runtime.o
