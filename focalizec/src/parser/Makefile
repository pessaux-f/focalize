# $Id: Makefile,v 1.28 2007-10-29 11:22:33 weis Exp $

include ../Makefile.config
CAML_COMP_FLAGS = -I ../../focc/ -I ../basement

BYTOBJS = lexer.cmo parser.cmo parse_file.cmo dump_ptree.cmo sourcify.cmo


all:: $(BYTOBJS)

doc:: $(BYTOBJS)
	$(CAMLDOC) $(CAML_COMP_FLAGS) -html -d odoc $(BYTOBJS:.cmo=.ml)

clean::
	$(RM) *~ *.cm* log lexer.ml lexer.mli
	$(RM) parser.ml parser.mli parser.conflicts parser.output
	$(RM) odoc/*

depend:
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

.mll.ml:
	$(CAMLLEX) $<

lexer.cmo: ../basement/parsetree.cmi parser.cmi

parser.mli: parser.ml

include .depend
