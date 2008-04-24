# $Id: Makefile,v 1.31 2008-04-24 13:30:41 pessaux Exp $

include ../Makefile.config
CAML_COMP_FLAGS = -I ../basement

BYTOBJS = lexer.cmo parser.cmo parse_file.cmo dump_ptree.cmo sourcify.cmo


all:: $(BYTOBJS)

doc:: $(BYTOBJS)
	$(CAMLDOC) $(CAML_COMP_FLAGS) -html -d odoc $(BYTOBJS:.cmo=.ml)

clean::
	$(RM) *~ *.cm* log lexer.ml lexer.mli
	$(RM) parser.ml parser.mli parser.conflicts parser.output
	$(RM) odoc/*

depend: lexer.ml parser.ml
	$(CAMLDEP) $(CAML_COMP_FLAGS) *.mli *.ml > ./.depend

explain:
	menhir --explain parser.mly > menhir.out 2>&1


.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cmx

.ml.cmo:
	$(CAMLBYT) $(CAML_COMP_FLAGS) -c $<

.mli.cmi:
	$(CAMLBYT) $(CAML_COMP_FLAGS) -c $<

.ml.cmx:
	$(CAMLBIN) $(CAML_COMP_FLAGS) -c $<

.mly.ml:
	$(CAMLYACC) $<

.mly.mli:
	$(CAMLYACC) $<

.mll.ml:
	$(CAMLLEX) $<

lexer.cmi: ../basement/parsetree.cmi parser.cmi lexer.ml

parser.cmi: parser.ml

parser.mli: parser.ml

include .depend
