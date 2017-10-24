all : main


type :
	ocamlbuild -use-ocamlfind types.native


type_checker : type
	ocamlbuild -use-ocamlfind typeChecker.native

treePrinter : type
	ocamlbuild -use-ocamlfind treePrinter.native

arbresSamples : type
	ocamlbuild -use-ocamlfind arbresSamples.native

script : type_checker treePrinter arbresSamples
	ocamlbuild -use-ocamlfind script.native

main : type_checker treePrinter
	ocamlbuild -use-ocamlfind main.native

clean: 
	ocamlbuild -clean
