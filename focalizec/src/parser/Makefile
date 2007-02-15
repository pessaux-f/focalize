all: parsetree.cmi lexer.ml parser.ml lexer.cmo parser.cmo 

lexer.ml: lexer.mll parser.mly
	ocamllex lexer.mll

parser.ml: parser.mly
	ocamlyacc parser.mly

clean::
	rm *~ *.cm* lexer.ml parser.ml parser.mli lexer.mli


.SUFFIXES: .cmo .cmi .ml .mll .mly

.ml.cmo:
	ocamlc -c $<

.mli.cmi:
	ocamlc -c $<

lexer.cmo: parser.cmi

parser.cmi: parser.mli
	ocamlc -c parser.mli

parsetree.cmi: parsetree.mli
	ocamlc -c parsetree.mli

#location.cmi: location.mli
#	ocamlc -c location.mli

#location.cmo: location.ml
#	ocamlc -c location.ml

