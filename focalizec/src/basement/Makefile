# $Id: Makefile,v 1.2 2007-08-15 17:00:01 pessaux Exp $

include ../Makefile.config
CAML_COMP_FLAGS = -I ../../focc/ 

BYTOBJS = configuration.cmo handy.cmo location.cmo types.cmo files.cmo \
	parsetree_utils.cmo


all:: $(BYTOBJS)

clean::
	$(RM) *~ *.cm* log
	$(RM) doc/*

doc:: $(BYTOBJS)
	$(CAMLDOC) -html -d doc $(BYTOBJS:.cmo=.ml)

depend:
	$(CAMLDEP) $(CAML_COMP_FLAGS) *.mli *.ml > ./.depend


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

include .depend
