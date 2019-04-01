.PHONY: default
default: my-language.native

%.native:
	ocamlbuild -use-ocamlfind $@
	mv $@ $*
