all : main


type :
	ocamlbuild -use-ocamlfind types.native

parser : type
	ocamlbuild -use-ocamlfind parser.native


type_checker : type
	ocamlbuild -use-ocamlfind typeChecker.native

treePrinter : type
	ocamlbuild -use-ocamlfind treePrinter.native


script : type_checker treePrinter parser type
	ocamlbuild -use-ocamlfind script.byte
	js_of_ocaml script.byte

main : type_checker treePrinter parser
	ocamlbuild -use-ocamlfind main.native

clean: 
	ocamlbuild -clean
	rm *.cmi *.cmo *.byte *.js

