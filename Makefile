.PHONY: all clean test

all: btp

btp:
	$(MAKE) -C src $@
	ln -s src/$@ .

clean:
	$(MAKE) -C src $@
	$(RM) btp

test: btp
	$(MAKE) -C test
