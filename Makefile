all : type_checker


type :
	ocamlbuild -use-ocamlfind types.native


type_checker : type
	ocamlbuild -use-ocamlfind typeChecker.native


clean: 
	ocamlbuild -clean
