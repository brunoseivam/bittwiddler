.PHONY: all clean
all: scanner.ml

scanner.ml: scanner.mll
	ocamllex $< -o $@

clean:
	@-rm scanner.ml
