# $Id: Makefile,v 1.1 2007-11-21 16:34:15 pessaux Exp $

include ../Makefile.config
CAML_COMP_FLAGS = -I ../../focc/ -I ../basement -I ../parser -I ../typing -I ../mlcodegen

BYTOBJS = species_coq_generation.cmo main_coq_generation.cmo

all:: $(BYTOBJS)

doc:: $(BYTOBJS)
	$(CAMLDOC) $(CAML_COMP_FLAGS) -html -d odoc $(BYTOBJS:.cmo=.ml)

clean::
	$(RM) *~ *.cm* log
	$(RM) odoc/*

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
