# $Id: Makefile,v 1.12 2008-04-17 13:18:16 bartlett Exp $

include ../Makefile.config
FOCC = ../toplevel/check_file
# Jut redefine to remove the warnings because FoCaL has some warnings
# in its generated code. Especially "Warning Z: unused variable ..."
# because FoCaL doesn't have the "_" ident which would be usefull in
# the case of let foo (x in unit) = ...
CAMLBYT = ocamlc
CAMLBIN = ocamlc
COQC = coqc

FOC_COMP_FLAGS =

FOCSRC = basics.foc \
	sets_orders.foc \
	iterators.foc \
	additive_law.foc \
	multiplicative_law.foc \
	weak_structures.foc \
	wellfounded.foc
# rings_fields.foc

FOCMLS = $(FOCSRC:.foc=.ml)
FOCCMOS = $(FOCMLS:.ml=.cmo)
FOCVS = $(FOCSRC:.foc=.v)
FOCVOS = $(FOCVS:.v=.vo)
EXTERNALOBJS = ml_builtins.cmo sets_orders_externals.cmo \
	weak_structures_externals.cmo \
	coq_builtins.vo weak_structures_externals.vo



all:: $(EXTERNALOBJS) $(FOCMLS) $(FOCVS) basics_special_case $(FOCCMOS) $(FOCVOS)

doc:: $(BYTOBJS)
	$(CAMLDOC) -html -d odoc $(BYTOBJS:.cmo=.ml)

clean::
	$(RM) *~ *.cm* log *.fo $(FOCMLS) $(FOCVS) $(FOCVOS) $(EXTERNALOBJS)
	$(RM) odoc/*

depend:
	$(CAMLDEP) *.mli *.ml > ./.depend

basics_special_case:
	$(RM) -f basics.v
	$(LN) -s ../tmp_basics.v basics.v
	$(COQC) basics.v

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cmx .foc .fo .v .vo

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

.v.vo:
	$(COQC) $<

include .depend
