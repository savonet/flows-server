all:
	@dune build

run:
	$(MAKE) -C src $@
