#**********************************************************************#
#                                                                      #
#                        FoCaLize compiler                             #
#                                                                      #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.6 2008-10-28 15:54:25 weis Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

BYT_FOCALIZE_COMPILER = $(ROOT_DIR)/toplevel/focalizec.byt
BIN_FOCALIZE_COMPILER = $(BYT_FOCALIZE_COMPILER:.byt=.bin)

FOCALIZE_COMPILER = $(BIN_FOCALIZE_COMPILER)

FOCALIZE_COMP_FLAGS = -no-stdlib-path -I ../../stdlib

#From Makefile.config FOCALIZEC = $(FOCALIZE_COMPILER) $(FOCALIZE_COMP_FLAGS)

COQ_INCLUDES = -I ../../../zenon/ -I ../../stdlib

#	products.foc
FOCALIZE_SRC = \
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

FOCALIZE_SRC_UNSAFE = \
	big_integers.foc
#	parse_poly.foc polys_abstract.foc polys_concrete finite_fields fractions finite_factorize

FOCALIZE_MLS = $(FOCALIZE_SRC:.foc=.ml)
FOCALIZE_MLS_UNSAFE = $(FOCALIZE_SRC_UNSAFE:.foc=.ml)

FOCALIZE_BYT_OBJS = $(FOCALIZE_MLS:.ml=.cmo)
FOCALIZE_BYT_OBJS_UNSAFE = $(FOCALIZE_MLS_UNSAFE:.ml=.cmo)

FOCALIZE_BIN_OBJS = $(FOCALIZE_BYT_OBJS:.cmo=.cmx)
FOCALIZE_BIN_OBJS_UNSAFE = $(FOCALIZE_BYT_OBJS_UNSAFE:.cmo=.cmx)

FOCALIZE_ZVS = $(FOCALIZE_SRC:.foc=.zv)
FOCALIZE_ZVS_UNSAFE = $(FOCALIZE_SRC_UNSAFE:.foc=.zv)

FOCALIZE_VS = $(FOCALIZE_ZVS:.zv=.v)
FOCALIZE_VS_UNSAFE = $(FOCALIZE_SRC_UNSAFE:.foc=.v)

FOCALIZE_VOS = $(FOCALIZE_VS:.v=.vo)
FOCALIZE_VOS_UNSAFE = $(FOCALIZE_SRC_UNSAFE:.foc=.vo)

EXTERNAL_BYT_OBJS =\
	weak_structures_externals.cmo \
	random_foc_externals.cmo

EXTERNAL_BIN_OBJS = $(EXTERNAL_BYT_OBJS:.cmo=.cmx)

EXTERNAL_VOS =\
	weak_structures_externals.vo \
	random_foc_externals.vo array_foc_externals.vo

include $(ROOT_DIR)/Makefile.common

lib: all

byt:: \
      $(EXTERNAL_BYT_OBJS) $(EXTERNAL_VOS)\
      $(FOCALIZE_MLS) $(FOCALIZE_MLS_UNSAFE)\
      $(FOCALIZE_ZVS)\
      $(FOCALIZE_BYT_OBJS) $(FOCALIZE_BYT_OBJS_UNSAFE)\
      $(FOCALIZE_VS) $(FOCALIZE_VOS)

bin:: \
      $(EXTERNAL_BIN_OBJS) $(EXTERNAL_VOS)\
      $(FOCALIZE_MLS) $(FOCALIZE_MLS_UNSAFE)\
      $(FOCALIZE_ZVS)\
      $(FOCALIZE_BIN_OBJS) $(FOCALIZE_BIN_OBJS_UNSAFE)\
      $(FOCALIZE_VS) $(FOCALIZE_VOS)

install:: all
	echo "TODO"

clean::
	$(RM) $(FOCALIZE_MLS) $(FOCALIZE_ZVS) $(FOCALIZE_VS)
	$(RM) $(FOCALIZE_MLS_UNSAFE) $(FOCALIZE_ZVS_UNSAFE) $(FOCALIZE_VS_UNSAFE)

# Were not compiled to Coq in the previous FoCaL compiler...
big_integers.ml: big_integers.foc
	$(FOCALIZEC) --no-coq-code $<
parse_poly.ml: parse_poly.foc
	$(FOCALIZEC) --no-coq-code $<
polys_abstract.ml: polys_abstract.foc
	$(FOCALIZEC) --no-coq-code $<
#End temporary


$(BYT_FOCALIZE_COMPILER) $(BIN_FOCALIZE_COMPILER):
	cd $(ROOT_DIR); $(MAKE) all

# Should be generated ?
random_foc_externals.vo: ../../stdlib/basics.vo
array_foc_externals.vo: ../../stdlib/basics.vo
# End Should be generated ?
