all : add


anex :
	ocamlbuild -use-ocamlfind anex.byte

add : anex
	ocamlbuild -use-ocamlfind add.byte
	js_of_ocaml add.byte


clean: 
	ocamlbuild -clean
	rm *.cmi *.cmo *.js

