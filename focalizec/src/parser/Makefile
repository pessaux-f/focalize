CAMLBYT = ocamlc -warn-error A -g
CAMLBIN = ocamlopt -warn-error A

CAMLYAC = ocamlyacc -v
CAMLLEX = ocamllex

BYTOBJS=configuration.cmo \
   lexer.cmo parser.cmo parse_file.cmo \
   check_file.cmo

CAMLBYTLIBS=lexing.cmo parsing.cmo

RM = rm -rf

all:: $(BYTOBJS)
	$(CAMLBYT) -o check_file $(CAMLBYTLIBS) $(BYTOBJS)

clean::
	$(RM) *~ *.cm* log lexer.ml lexer.mli
	$(RM) parser.ml parser.mli parser.conflicts parser.output

depend:
	ocamldep *.mli *.ml > ./.depend

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cmx 

.ml.cmo:
	$(CAMLBYT) -c $<

.mli.cmi:
	$(CAMLBYT) -c $<

.ml.cmx:
	$(CAMLBIN) -c $<

.mly.ml:
	$(CAMLYAC) $<
#	(menhir --explain $< 2>&1) > log

.mll.ml:
	$(CAMLLEX) $<

lexer.cmo: parsetree.cmi parser.cmi

parser.mli: parser.ml

include .depend
