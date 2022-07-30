all:
	@dune build

run:
	$(MAKE) -C src $@

format:
	ocamlformat -i **/*.ml
