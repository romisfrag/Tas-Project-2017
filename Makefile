all : script


types.ml :
	ocamlbuild -use-ocamlfind types.native

parser.ml : types.ml
	ocamlbuild -use-ocamlfind parser.native


typeChecker.ml : types.ml
	ocamlbuild -use-ocamlfind typeChecker.native

treePrinter.ml : types.ml
	ocamlbuild -use-ocamlfind treePrinter.native


script.ml : typeChecker.ml treePrinter.ml parser.ml types.ml
	ocamlbuild -use-ocamlfind script.byte
	js_of_ocaml script.byte

clean: 
	ocamlbuild -clean
	rm *.cmi *.cmo *.byte *.js

