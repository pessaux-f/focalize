# (* $Id: Makefile,v 1.1 2007-07-27 13:54:19 pessaux Exp $ *) 

include ../Makefile.config
CAML_COMP_FLAGS = -I ../../focc/ -I ../basement -I ../parser

BYTOBJS = scope_env.cmo scoping.cmo

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
