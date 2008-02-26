# $Id: Makefile,v 1.4 2008-02-26 13:00:56 pessaux Exp $

include ../Makefile.config
FOCC = ../toplevel/check_file
# Jut redefine to remove the warnings because FoCaL has some warnings
# in its generated code. Especially "Warning Z: unused variable ..."
# because FoCaL doesn't have the "_" ident which would be usefull in
# the case of let foo (x in unit) = ...
CAMLBYT = ocamlc
CAMLBIN = ocamlc


FOC_COMP_FLAGS =

FOCSRC = basics.foc \
	sets_orders.foc \
	iterators.foc \
	additive_law.foc \
	multiplicative_law.foc \
	weak_structures.foc

FOCMLS = $(FOCSRC:.foc=.ml)
FOCCMOS = $(FOCMLS:.ml=.cmo)
FOCVS = $(FOCSRC:.foc=.v)
FOCVOS = $(FOCVS:.v=.vo)
EXTERNALOBJS = focvasives.cmo sets_orders_externals.cmo



all:: $(EXTERNALOBJS) $(FOCMLS) $(FOCVS) $(FOCCMOS)
	$(RM) -f basics.v
	$(LN) -s ../tmp_basics.v basics.v

doc:: $(BYTOBJS)
	$(CAMLDOC) -html -d odoc $(BYTOBJS:.cmo=.ml)

clean::
	$(RM) *~ *.cm* log *.fo $(FOCMLS) $(FOCVS) $(FOCVOS)
	$(RM) odoc/*

depend:
	$(CAMLDEP) *.mli *.ml > ./.depend

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cmx .foc .fo

.foc.fo .foc.ml .foc.v:
	$(FOCC) $(FOC_COMP_FLAGS) -c $<

.ml.cmo:
	$(CAMLBYT) -c $<

.mli.cmi:
	$(CAMLBYT) -c $<

.ml.cmx:
	$(CAMLBIN) -c $<

.mly.ml:
	$(CAMLYACC) $<

.mll.ml:
	$(CAMLLEX) $<


include .depend
