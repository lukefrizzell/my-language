.PHONY: default
default: tokens.native

%.native:
	ocamlbuild -use-ocamlfind $@
	mv $@ $*
