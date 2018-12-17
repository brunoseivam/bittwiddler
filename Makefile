.PHONY: all test clean

all: bittwiddler.native runtime.o gen_bin_data

gen_bin_data: gen_bin_data.c
	$(CC) -o $@ $<

test: all testall.sh
	./testall.sh

bittwiddler.native:
	opam config exec -- \
	ocamlbuild -use-ocamlfind bittwiddler.native

clean:
	ocamlbuild -clean
	-rm -f *diff *.ll *.err *.s *.out *.exe testall.log runtime.o gen_bin_data
