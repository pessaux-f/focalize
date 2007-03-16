CAMLBYT = ocamlc -warn-error A -g
CAMLBIN = ocamlopt -warn-error A

CAMLYAC = ocamlyacc -v
CAMLLEX = ocamllex

RM = rm -rf

all:: parsetree.cmi lexer.cmo parser.cmo 

clean::
	$(RM) *~ *.cm* lexer.ml parser.ml parser.mli lexer.mli

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cmx 

.ml.cmo:
	$(CAMLBYT) -c $<

.mli.cmi:
	$(CAMLBYT) -c $<

.ml.cmx:
	$(CAMLBIN) -c $<

.mly.cmo:
	$(CAMLYAC) $<
#	(menhir --explain $< 2>&1) > log
	$(CAMLBYT) -c $*.mli
	$(CAMLBYT) -c $*.ml

.mly.cmx:
	$(CAMLYAC) $<
	$(CAMLBIN) -c $*.mli
	$(CAMLBIN) -c $*.ml

.mly.cmi:
	$(CAMLYAC) $<
	$(CAMLBYT) -c $*.mli

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYAC) $<

lexer.cmo: parser.cmi
