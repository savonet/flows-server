all:
	@dune build

run:
	$(MAKE) -C src $@

format:
	@dune build @fmt --auto-promote
