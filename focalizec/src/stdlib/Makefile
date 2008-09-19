# $Id: Makefile,v 1.36 2008-09-18 23:16:00 weis Exp $

ROOTDIR = ..

include $(ROOTDIR)/Makefile.config

# Jut redefine to remove the warnings because FoCaL has some warnings
# in its generated code. Especially "Warning Z: unused variable ..."
# because FoCaL doesn't have the "_" ident which would be usefull in
# the case of let foo (x in unit) = ...
CAMLBYT = ocamlc -w A
CAMLBIN = ocamlc -w A

FOCC = ../toplevel/check_file -no-stdlib-path

ZVTOV = ../../zvtov/zvtov -zenon ../../zenon/zenon

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
	array_foc.foc \
	products_foc.foc \
	quotients.foc \
	small_integers.foc

FOCSRC_UNSAFE =	big_integers.foc \
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
	random_foc_externals.vo array_foc_externals.vo

include $(ROOTDIR)/Makefile.common

byt:: $(EXTERNALOBJS) $(FOCMLS) $(FOCMLS_UNSAFE) \
 $(FOCZVS) $(FOCCMOS) $(FOCCMOS_UNSAFE) $(FOCVS) $(FOCVOS)

install:: all
	echo "TODO"

clean::
	$(RM) $(FOCMLS) $(FOCZVS) $(FOCVS) $(FOCVOS)
	$(RM) $(EXTERNALOBJS)
	$(RM) $(FOCMLS_UNSAFE) $(FOCZVS_UNSAFE) $(FOCVS_UNSAFE) $(FOCVOS_UNSAFE)
	$(RM) *.mangled *.pfc

# Were not compiled to Coq in the previous FoCaL compiler...
big_integers.ml: big_integers.foc
	$(FOCC) --no-coq-code $<
parse_poly.ml: parse_poly.foc
	$(FOCC) --no-coq-code $<
polys_abstract.ml: polys_abstract.foc
	$(FOCC) --no-coq-code $<
#End temporary

# Special rules for exceptions that need to be compiled after basics.foc.
array_foc_externals.vo: basics.vo
random_foc_externals.vo: basics.vo
