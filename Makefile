all: ppx_to_prod.native

ppx_to_prod.native: ppx_to_prod.ml
	ocamlbuild -package compiler-libs.common ppx_to_prod.native
