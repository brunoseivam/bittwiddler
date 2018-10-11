.PHONY: all clean test

all: btp

btp:
	$(MAKE) -C src $@
	cp src/$@ .

clean:
	$(RM) btp

test: btp
	$(MAKE) -C test
