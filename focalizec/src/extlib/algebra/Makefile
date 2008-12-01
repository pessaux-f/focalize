#**********************************************************************#
#                                                                      #
#                        FoCaLize compiler                             #
#                                                                      #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                                                                      #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.16 2008-12-01 20:40:22 weis Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

LIB_INSTALL_DIR = $(LIB_DIR)/focalize/algebra

# [rr]: changed this to include standard library
CAML_COMP_FLAGS = -w A -warn-error Ae -dtypes -I $(ROOT_DIR)/stdlib


BYT_FOCALIZE_COMPILER = $(ROOT_DIR)/toplevel/focalizec.byt
BIN_FOCALIZE_COMPILER = $(BYT_FOCALIZE_COMPILER:.byt=.bin)

FOCALIZE_COMPILER = $(BIN_FOCALIZE_COMPILER)

FOCALIZE_COMP_FLAGS = -no-stdlib-path -I $(ROOT_DIR)/stdlib

#From Makefile.config FOCALIZEC = $(FOCALIZE_COMPILER) $(FOCALIZE_COMP_FLAGS)

COQ_INCLUDES = -I $(ROOT_DIR)/../zenon/ -I $(ROOT_DIR)/stdlib

#	products.fcl
FOCALIZE_SRC = \
	constants.fcl \
	additive_law.fcl \
	multiplicative_law.fcl \
	weak_structures.fcl \
	rings_fields.fcl \
	random_foc.fcl \
	integers.fcl \
	array_foc.fcl \
	products_foc.fcl \
	quotients.fcl

FOCALIZE_SRC_UNSAFE = \
	iterators.fcl \
	big_integers.fcl
#	parse_poly.fcl polys_abstract.fcl polys_concrete finite_fields fractions finite_factorize

FOCALIZE_MLS = $(FOCALIZE_SRC:.fcl=.ml)
FOCALIZE_MLS_UNSAFE = $(FOCALIZE_SRC_UNSAFE:.fcl=.ml)

FOCALIZE_BYT_OBJS = $(FOCALIZE_MLS:.ml=.cmo)
FOCALIZE_BYT_OBJS_UNSAFE = $(FOCALIZE_MLS_UNSAFE:.ml=.cmo)

FOCALIZE_BIN_OBJS = $(FOCALIZE_BYT_OBJS:.cmo=.cmx)
FOCALIZE_BIN_OBJS_UNSAFE = $(FOCALIZE_BYT_OBJS_UNSAFE:.cmo=.cmx)

FOCALIZE_ZVS = $(FOCALIZE_SRC:.fcl=.zv)
FOCALIZE_ZVS_UNSAFE = $(FOCALIZE_SRC_UNSAFE:.fcl=.zv)

FOCALIZE_VS = $(FOCALIZE_ZVS:.zv=.v)
FOCALIZE_VS_UNSAFE = $(FOCALIZE_SRC_UNSAFE:.fcl=.v)

FOCALIZE_VOS = $(FOCALIZE_VS:.v=.vo)
FOCALIZE_VOS_UNSAFE = $(FOCALIZE_SRC_UNSAFE:.fcl=.vo)

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

install::
	$(MKDIR) $(LIB_INSTALL_DIR)
	@if [ "$(FOCALIZE_BYT_OBJS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BYT_OBJS) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(FOCALIZE_BYT_OBJS_UNSAFE)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BYT_OBJS_UNSAFE) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(FOCALIZE_BIN_OBJS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BIN_OBJS) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(FOCALIZE_BIN_OBJS_UNSAFE)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BIN_OBJS_UNSAFE) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(FOCALIZE_VOS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_VOS) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(EXTERNAL_BYT_OBJS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_BYT_OBJS) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(EXTERNAL_BIN_OBJS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_BIN_OBJS) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(EXTERNAL_VOS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_VOS) $(LIB_INSTALL_DIR)/ ; \
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
big_integers.ml: big_integers.fcl
	$(FOCALIZEC) --no-coq-code $<
parse_poly.ml: parse_poly.fcl
	$(FOCALIZEC) --no-coq-code $<
polys_abstract.ml: polys_abstract.fcl
	$(FOCALIZEC) --no-coq-code $<
#End temporary

# Should be generated ?
random_foc_externals.vo: $(ROOT_DIR)/stdlib/basics.vo
array_foc_externals.vo: $(ROOT_DIR)/stdlib/basics.vo
# End Should be generated ?
