.PHONY: build
build:
	ocamlopt -o simplify \
		str.cmxa \
		bark.mli bark.ml \
		utils.ml \
		lang.ml \
		parse.mli parse.ml \
		unparse.ml \
		demo.ml

.PHONY: test
test:
	ocamlopt -o test \
		str.cmxa \
		bark.mli bark.ml \
		utils.ml \
		lang.ml \
		parse.mli parse.ml \
		unparse.ml \
		tests.ml
	./test

.PHONY: clean
clean:
	rm *.cmi *.cmx *.o simplify test