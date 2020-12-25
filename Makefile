install:
	opam install -y yojson lwt cohttp-lwt-unix cohttp ocurl ounit

test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

clean:
	ocamlbuild -clean
	rm -f *.cm*
	rm -f *.byte
	rm -f finalsrc.zip

repl:
	ocamlbuild -use-ocamlfind repl.byte && ./repl.byte

zip:
	zip finalsrc.zip *.ml*

compile:
	ocamlbuild -use-ocamlfind lexer.byte eval.cmo comb_eval.cmo mod_arith.cmo linear_alg.cmo rsa.cmo simpl_arith.cmo repl.byte

server:
	make compile
	ocamlbuild \
	-pkgs lwt,cohttp,cohttp-lwt-unix,yojson,curl,str \
		server.byte 
	./server.byte
