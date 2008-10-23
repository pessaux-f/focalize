#**********************************************************************#
#                                                                      #
#                        FoCaL compiler                                #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.5 2008-10-23 13:09:43 doligez Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

# Just redefine to remove the warnings because FoCaL has some warnings
# in its generated code. Especially "Warning Z: unused variable ..."
# because FoCaL doesn't have the "_" ident which would be usefull in
# the case of let foo (x in unit) = ...
CAMLBYT = ocamlc -w A -I ../../stdlib
CAMLBIN = ocamlc -w A -I ../../stdlib

BYT_FOCAL_COMPILER = $(ROOT_DIR)/toplevel/focalizec.byt
BIN_FOCAL_COMPILER = $(BYT_FOCAL_COMPILER:.byt=.bin)

FOCALIZE_COMPILER = $(BYT_FOCAL_COMPILER)

FOCALIZE_COMP_FLAGS = -no-stdlib-path -I ../../stdlib

#From Makefile.config FOCALC = $(FOCAL_COMPILER) $(FOCAL_COMP_FLAGS)

ZVTOV = ../../../zvtov/zvtov -new -zenon ../../../zenon/zenon

COQ_COMP_FLAGS = -I ../../../zenon/ -I ../../stdlib

#	products.foc
FOCAL_SRC = \
	iterators.foc \
	additive_law.foc \
	multiplicative_law.foc \
	weak_structures.foc \
	rings_fields.foc \
	random_foc.foc \
	integers.foc \
	array_foc.foc \
	products_foc.foc \
	quotients.foc \
	small_integers.foc

FOCAL_SRC_UNSAFE = \
	big_integers.foc
#	parse_poly.foc polys_abstract.foc polys_concrete finite_fields fractions finite_factorize

FOCAL_MLS = $(FOCAL_SRC:.foc=.ml)
FOCAL_MLS_UNSAFE = $(FOCAL_SRC_UNSAFE:.foc=.ml)

FOCAL_BYTOBJS = $(FOCAL_MLS:.ml=.cmo)
FOCAL_BYTOBJS_UNSAFE = $(FOCAL_MLS_UNSAFE:.ml=.cmo)

FOCAL_BINOBJS = $(FOCAL_BYTOBJS:.cmo=.cmx)
FOCAL_BINOBJS_UNSAFE = $(FOCAL_BYTOBJS_UNSAFE:.cmo=.cmx)

FOCAL_ZVS = $(FOCAL_SRC:.foc=.zv)
FOCAL_ZVS_UNSAFE = $(FOCAL_SRC_UNSAFE:.foc=.zv)

FOCAL_VS = $(FOCAL_ZVS:.zv=.v)
FOCAL_VS_UNSAFE = $(FOCAL_SRC_UNSAFE:.foc=.v)

FOCAL_VOS = $(FOCAL_VS:.v=.vo)
FOCAL_VOS_UNSAFE = $(FOCAL_SRC_UNSAFE:.foc=.vo)

EXTERNAL_BYTOBJS =\
	weak_structures_externals.cmo \
	random_foc_externals.cmo

EXTERNAL_BINOBJS = $(EXTERNAL_BYTOBJS:.cmo=.cmx)

EXTERNAL_VOS =\
	weak_structures_externals.vo \
	random_foc_externals.vo array_foc_externals.vo

include $(ROOT_DIR)/Makefile.common

byt:: \
      $(EXTERNAL_BYTOBJS) $(EXTERNAL_VOS)\
      $(FOCAL_MLS) $(FOCAL_MLS_UNSAFE)\
      $(FOCAL_ZVS)\
      $(FOCAL_BYTOBJS) $(FOCAL_BYTOBJS_UNSAFE)\
      $(FOCAL_VS) $(FOCAL_VOS)

bin:: \
      $(EXTERNAL_BINOBJS) $(EXTERNAL_VOS)\
      $(FOCAL_MLS) $(FOCAL_MLS_UNSAFE)\
      $(FOCAL_ZVS)\
      $(FOCAL_BINOBJS) $(FOCAL_BINOBJS_UNSAFE)\
      $(FOCAL_VS) $(FOCAL_VOS)

install:: all
	echo "TODO"

clean::
	$(RM) $(FOCAL_MLS) $(FOCAL_ZVS) $(FOCAL_VS)
	$(RM) $(FOCAL_MLS_UNSAFE) $(FOCAL_ZVS_UNSAFE) $(FOCAL_VS_UNSAFE)

# Were not compiled to Coq in the previous FoCaL compiler...
big_integers.ml: big_integers.foc
	$(FOCALIZEC) --no-coq-code $<
parse_poly.ml: parse_poly.foc
	$(FOCALIZEC) --no-coq-code $<
polys_abstract.ml: polys_abstract.foc
	$(FOCALIZEC) --no-coq-code $<
#End temporary


$(BYT_FOCAL_COMPILER) $(BIN_FOCAL_COMPILER):
	cd $(ROOT_DIR); $(MAKE) all

random_foc_externals.vo: ../../stdlib/basics.vo
array_foc_externals.vo: ../../stdlib/basics.vo
