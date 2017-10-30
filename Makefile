all : script


types :
	ocamlbuild -use-ocamlfind types.native

parser : types
	ocamlbuild -use-ocamlfind parser.native


typeChecker : types
	ocamlbuild -use-ocamlfind typeChecker.native

treePrinter : types
	ocamlbuild -use-ocamlfind treePrinter.native


script : typeChecker treePrinter parser types
	ocamlbuild -use-ocamlfind script.byte
	js_of_ocaml script.byte

clean: 
	ocamlbuild -clean
	rm *.cmi *.cmo *.byte *.js

