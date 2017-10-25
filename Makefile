all : main


type :
	ocamlbuild -use-ocamlfind types.native

parser : type
	ocamlbuild -use-ocamlfind parser.native


type_checker : type
	ocamlbuild -use-ocamlfind typeChecker.native

treePrinter : type
	ocamlbuild -use-ocamlfind treePrinter.native


script : type_checker treePrinter 
	ocamlfind ocamlc -I _build -package js_of_ocaml -package js_of_ocaml.ppx \
          -linkpkg -o script.byte script.ml
	js_of_ocaml script.byte

main : type_checker treePrinter parser
	ocamlbuild -use-ocamlfind main.native

clean: 
	ocamlbuild -clean
	rm script.cmi script.cmo script.byte script.byte script.js

