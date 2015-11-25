main:
	ocamlbuild main.native

top:
	ocamlbuild -use-menhir top.top;
	./top.top -I _build/

runtop:
	./top.top -I _build/

clean:
	rm -rf _build main.native top.top
