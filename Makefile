main:
	ocamlbuild main.native

toplevel:
	ocamlbuild -use-menhir top.top;
	./top.top -I _build/

runtop:
	./top.top -I _build/

clean:
	rm -rf _build main.native top.top
