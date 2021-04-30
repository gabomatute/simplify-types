.PHONY: build
build:
	ocamlopt -o simplify \
		str.cmxa \
		bark.mli bark.ml \
		utils.ml \
		lang.ml \
		parse.mli parse.ml \
		demo.ml
