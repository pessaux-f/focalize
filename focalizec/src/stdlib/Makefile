# $Id: Makefile,v 1.17 2008-04-30 08:54:36 pessaux Exp $

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
#	wellfounded.foc

FOCSRC_UNSAFE =	rings_fields.foc \
	random_foc.foc \
	integers.foc \
	array_foc.foc \
	products_foc.foc \
	quotients.foc \
	small_integers.foc \
	big_integers.foc

FOCMLS = $(FOCSRC:.foc=.ml)
FOCMLS_UNSAFE = $(FOCSRC_UNSAFE:.foc=.ml)

FOCCMOS = $(FOCMLS:.ml=.cmo)
FOCCMOS_UNSAFE = $(FOCMLS_UNSAFE:.ml=.cmo)

FOCVS = $(FOCSRC:.foc=.v)

FOCVOS = $(FOCVS:.v=.vo)

EXTERNALOBJS = ml_builtins.cmo sets_orders_externals.cmo \
	weak_structures_externals.cmo \
	coq_builtins.vo weak_structures_externals.vo wellfounded_externals.vo \
	random_foc_externals.cmo




all:: $(EXTERNALOBJS) $(FOCMLS) $(FOCMLS_UNSAFE) $(FOCVS) basics_special_case $(FOCCMOS) $(FOCCMOS_UNSAFE) $(FOCVOS)

doc:: $(BYTOBJS)
	$(CAMLDOC) -html -d odoc $(BYTOBJS:.cmo=.ml)

clean::
	$(RM) *~ *.cm* log *.fo $(FOCMLS) $(FOCVS) $(FOCVOS) $(EXTERNALOBJS)
	$(RM) $(FOCMLS_UNSAFE) $(FOCVS_UNSAFE) $(FOCVOS_UNSAFE)
	$(RM) *.mangled odoc/*

depend:
	$(CAMLDEP) *.mli *.ml > ./.depend

basics_special_case:
	$(RM) -f basics.v
	$(LN) -s ../tmp_basics.v basics.v
	$(COQC) basics.v

# Temporary for no Coq...
rings_fields.ml: rings_fields.foc
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code -c $<
random_foc.ml: random_foc.foc
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code -c $<
integers.ml: integers.foc
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code -c $<
array_foc.ml: array_foc.foc
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code -c $<
products_foc.ml: products_foc.foc
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code -c $<
quotients.ml: quotients.foc
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code -c $<
small_integers.ml: small_integers.foc
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code -c $<
big_integers.ml: big_integers.foc
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code -c $<
#End temporary


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
