# $Id: Makefile,v 1.21.2.2 2008-08-04 15:37:58 blond Exp $

include ../Makefile.config
FOCC = ../toplevel/check_file -no-stdlib-path
# Jut redefine to remove the warnings because FoCaL has some warnings
# in its generated code. Especially "Warning Z: unused variable ..."
# because FoCaL doesn't have the "_" ident which would be usefull in
# the case of let foo (x in unit) = ...
CAMLBYT = ocamlc
CAMLBIN = ocamlc
COQC = coqc
CC = gcc

CC_FLAGS =

FOC_COMP_FLAGS = --c-code

FOCSRC = basics.foc \
	sets_orders.foc \
	iterators.foc \
	additive_law.foc \
	multiplicative_law.foc \
	weak_structures.foc \
	wellfounded.foc

FOCSRC_UNSAFE =	rings_fields.foc \
	random_foc.foc \
	integers.foc \
	array_foc.foc \
	products_foc.foc \
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

FOCVS = $(FOCSRC:.foc=.v)

FOCVOS = $(FOCVS:.v=.vo)

FOCCS = $(FOCSRC:.foc=.c)
FOCCS_UNSAFE = $(FOCSRC_UNSAFE:.foc=.c)

FOCOS = $(FOCCS:.c=.o)
FOCOS_UNSAFE = $(FOCCS_UNSAFE:.c=.o)

EXTERNALOBJS = ml_builtins.cmo sets_orders_externals.cmo \
	weak_structures_externals.cmo \
	coq_builtins.vo weak_structures_externals.vo wellfounded_externals.vo \
	random_foc_externals.cmo




all:: $(EXTERNALOBJS) $(FOCCS) $(FOCOS)

doc:: $(BYTOBJS)
	$(CAMLDOC) -html -d odoc $(BYTOBJS:.cmo=.ml)

install: all
	echo "TODO"

clean::
	$(RM) *~ *.cm* log *.fo $(FOCMLS) $(FOCVS) $(FOCVOS) $(EXTERNALOBJS)
	$(RM) $(FOCCS) $(FOCOS)
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
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code $<
random_foc.ml: random_foc.foc
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code $<
integers.ml: integers.foc
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code $<
array_foc.ml: array_foc.foc
	$(FOCC) $(FOC_COMP_FLAGS) --no-coq-code $<
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


.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cmx .foc .fo .v .vo .h .c .o

.foc.fo .foc.ml .foc.v .foc.c:
	$(FOCC) $(FOC_COMP_FLAGS) $<

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

.c.o:
	$(CC) $(CC_FLAGS) -c $<

include .depend
