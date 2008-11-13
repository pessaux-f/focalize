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

# $Id: Makefile,v 1.10 2008-11-13 10:37:41 pessaux Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config
include $(ROOT_DIR)/Makefile.install

# [rr]: changed this to include standard library
CAML_COMP_FLAGS = -w A -warn-error Ae -dtypes -I $(ROOT_DIR)/stdlib


BYT_FOCALIZE_COMPILER = $(ROOT_DIR)/toplevel/focalizec.byt
BIN_FOCALIZE_COMPILER = $(BYT_FOCALIZE_COMPILER:.byt=.bin)

FOCALIZE_COMPILER = $(BIN_FOCALIZE_COMPILER)

FOCALIZE_COMP_FLAGS = -no-stdlib-path -I $(ROOT_DIR)/stdlib

#From Makefile.config FOCALIZEC = $(FOCALIZE_COMPILER) $(FOCALIZE_COMP_FLAGS)

COQ_INCLUDES = -I $(ROOT_DIR)/../zenon/ -I $(ROOT_DIR)/stdlib

#	products.foc
FOCALIZE_SRC = \
	constants.foc \
	additive_law.foc \
	multiplicative_law.foc \
	weak_structures.foc \
	rings_fields.foc \
	random_foc.foc \
	integers.foc \
	array_foc.foc \
	products_foc.foc \
	quotients.foc \


FOCALIZE_SRC_UNSAFE = \
	iterators.foc \
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
	$(MKDIR) $(FOC_INSTALL_LIB)/extlib/algebra
	@if [ "$(FOCALIZE_BYT_OBJS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BYT_OBJS) $(FOC_INSTALL_LIB)/extlib/algebra ; \
	fi
	@if [ "$(FOCALIZE_BYT_OBJS_UNSAFE)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BYT_OBJS_UNSAFE) $(FOC_INSTALL_LIB)/extlib/algebra ; \
	fi
	@if [ "$(FOCALIZE_BIN_OBJS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BIN_OBJS) $(FOC_INSTALL_LIB)/extlib/algebra ; \
	fi
	@if [ "$(FOCALIZE_BIN_OBJS_UNSAFE)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BIN_OBJS_UNSAFE) $(FOC_INSTALL_LIB)/extlib/algebra ; \
	fi
	@if [ "$(FOCALIZE_VOS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_VOS) $(FOC_INSTALL_LIB)/extlib/algebra ; \
	fi
	@if [ "$(FOCALIZE_VOS_UNSAFE)" != "" ] ; then \
	  $(CP) $(FOCALIZE_VOS_UNSAFE) $(FOC_INSTALL_LIB)/extlib/algebra ; \
	fi
	@if [ "$(EXTERNAL_BYT_OBJS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_BYT_OBJS) $(FOC_INSTALL_LIB)/extlib/algebra ; \
	fi
	@if [ "$(EXTERNAL_BIN_OBJS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_BIN_OBJS) $(FOC_INSTALL_LIB)/extlib/algebra ; \
	fi
	@if [ "$(EXTERNAL_VOS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_VOS) $(FOC_INSTALL_LIB)/extlib/algebra ; \
	fi


clean::
	$(RM) $(FOCALIZE_MLS) $(FOCALIZE_ZVS) $(FOCALIZE_VS)
	$(RM) $(FOCALIZE_BYT_OBJS) $(FOCALIZE_BIN_OBJS) $(FOCALIZE_VOS)
	$(RM) $(FOCALIZE_MLS_UNSAFE) $(FOCALIZE_ZVS_UNSAFE) $(FOCALIZE_VS_UNSAFE)
	$(RM) $(FOCALIZE_BYT_OBJS_UNSAFE) $(FOCALIZE_BIN_OBJS_UNSAFE) $(FOCALIZE_VOS_UNSAFE)
	$(RM) $(EXTERNAL_BYT_OBJS) $(EXTERNAL_BIN_OBJS) $(EXTERNAL_VOS)
	$(RM) big_integers.ml parse_poly.ml polys_abstract.ml
	$(RM) big_integers.cm* parse_poly.cm* polys_abstract.cm*
	$(RM) big_integers.o parse_poly.o polys_abstract.o


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
random_foc_externals.vo: $(ROOT_DIR)/stdlib/basics.vo
array_foc_externals.vo: $(ROOT_DIR)/stdlib/basics.vo
# End Should be generated ?
