OCAMLC=ocamlc -g

all: run

run: lexer.cmo parser.cmo k.cmo pp.cmo main.cmo
	$(OCAMLC) -o run lexer.cmo pp.cmo parser.cmo k.cmo main.cmo

ex1: run
	cat examples/test$(NUM).k-
	./run examples/test$(NUM).k-

ex2: run
	./run ex2.k-

ex3: run
	./run ex3.k-

k.cmo : k.ml
	$(OCAMLC) -c k.ml

pp.cmo : pp.ml k.cmo
	$(OCAMLC) -c pp.ml

parser.ml: parser.mly k.cmo
	ocamlyacc parser.mly

parser.mli: parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.mli
	$(OCAMLC) -c parser.mli

parser.cmo: parser.ml parser.cmi
	$(OCAMLC) -c parser.ml

main.cmo : k.cmo main.ml
	$(OCAMLC) -c main.ml

lexer.cmo: lexer.ml
	$(OCAMLC) -c lexer.ml

lexer.ml: lexer.mll parser.cmo
	ocamllex lexer.mll

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo
