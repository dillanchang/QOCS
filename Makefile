default:
	ocamlbuild -pkgs str,ANSITerminal main.byte && ./main.byte

test:
	ocamlbuild -pkgs oUnit,str,ANSITerminal test.byte && ./test.byte

clean:
	ocamlbuild -clean
