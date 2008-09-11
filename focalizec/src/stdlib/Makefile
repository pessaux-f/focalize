# $Id: Makefile,v 1.31 2008-09-11 09:26:21 pessaux Exp $

include ../Makefile.config
FOCC = ../toplevel/check_file -no-stdlib-path
# Jut redefine to remove the warnings because FoCaL has some warnings
# in its generated code. Especially "Warning Z: unused variable ..."
# because FoCaL doesn't have the "_" ident which would be usefull in
# the case of let foo (x in unit) = ...
CAMLBYT = ocamlc
CAMLBIN = ocamlc
COQC = coqc
ZVTOV = ../../zvtov/zvtov -zenon ../../zenon/zenon

FOC_COMP_FLAGS =
COQ_COMP_FLAGS = -I ../../zenon/

FOCSRC = basics.foc \
	sets_orders.foc \
	iterators.foc \
	additive_law.foc \
	multiplicative_law.foc \
	weak_structures.foc \
	wellfounded.foc \
	rings_fields.foc \
	random_foc.foc \
	integers.foc \
	array_foc.foc

FOCSRC_UNSAFE = products_foc.foc \
	quotients.foc \
	small_integers.foc \
	big_integers.foc \
	parse_poly.foc \
	ensembles_finis.foc
#	polys_abstract.foc polys_concrete finite_fields fractions finite_factorize

FOCMLS = $(FOCSRC:.foc=.ml)
FOCMLS_UNSAFE = $(FOCSRC_UNSAFE:.foc=.ml)

FOCCMOS = $(FOCMLS:.ml=.cmo)
FOCCMOS_UNSAFE = $(FOCMLS_UNSAFE:.ml=.cmo)

FOCZVS = $(FOCSRC:.foc=.zv)
FOCZVS_UNSAFE = $(FOCSRC_UNSAFE:.foc=.zv)

FOCVS = $(FOCZVS:.zv=.v)
FOCVS_UNSAFE = $(FOCSRC_UNSAFE:.foc=.v)

FOCVOS = $(FOCVS:.v=.vo)
FOCVOS_UNSAFE = $(FOCSRC_UNSAFE:.foc=.vo)

EXTERNALOBJS = ml_builtins.cmo sets_orders_externals.cmo \
	weak_structures_externals.cmo \
	coq_builtins.vo weak_structures_externals.vo \
	wellfounded_externals.vo random_foc_externals.cmo \
	array_foc_externals.vo




all:: $(EXTERNALOBJS) $(FOCMLS) $(FOCMLS_UNSAFE) $(FOCZVS) $(FOCCMOS) $(FOCCMOS_UNSAFE) $(FOCVS) $(FOCVOS)

doc:: $(BYTOBJS)
	$(CAMLDOC) -html -d odoc $(BYTOBJS:.cmo=.ml)

install: all
	echo "TODO"

clean::
	$(RM) *~ *.cm* log *.fo $(FOCMLS) $(FOCZVS) $(FOCVS) $(FOCVOS)
	$(RM) $(EXTERNALOBJS)
	$(RM) $(FOCMLS_UNSAFE) $(FOCZVS_UNSAFE) $(FOCVS_UNSAFE) $(FOCVOS_UNSAFE)
	$(RM) *.mangled odoc/* *.pfc

depend:
	$(CAMLDEP) *.mli *.ml > ./.depend


# Temporary for no Coq...
products_foc.ml: products_foc.foc
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code $<
quotients.ml: quotients.foc
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code $<
small_integers.ml: small_integers.foc
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code $<
big_integers.ml: big_integers.foc
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code $<
parse_poly.ml: parse_poly.foc
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code $<
polys_abstract.ml: polys_abstract.foc
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code $<
#End temporary

# Special rules for exceptions.
array_foc_externals.vo: basics.vo


.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cmx .foc .fo .zv .v .vo

.foc.fo .foc.ml .foc.zv:
	$(FOCC) $(FOC_COMP_FLAGS) $<

.zv.v:
	$(ZVTOV) -new $<

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
	$(COQC) $(COQ_COMP_FLAGS) $<

include .depend
