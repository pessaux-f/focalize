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

# $Id: Makefile,v 1.55 2008-11-04 11:31:33 pessaux Exp $

ROOT_DIR = ..

include $(ROOT_DIR)/Makefile.config

FOCALIZE_COMPILER = $(ROOT_DIR)/toplevel/focalizec

FOCALIZE_COMP_FLAGS = -no-stdlib-path

#From Makefile.config FOCALIZEC = $(FOCALIZE_COMPILER) $(FOCALIZE_COMP_FLAGS)

COQ_INCLUDES = -I ../../zenon/

FOCALIZE_SRC = basics.foc \
	sets.foc\
	products.foc \
	lattices.foc\
	orders.foc \
	ord_n_latt.foc \
	sets_orders.foc \
	wellfounded.foc \
	sums.foc

FOCALIZE_SRC_UNSAFE = \
	ensembles_finis.foc

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
  ml_builtins.cmo sets_externals.cmo sets_orders_externals.cmo
#  weak_structures_externals.cmo
#  random_foc_externals.cmo

EXTERNAL_BIN_OBJS = $(EXTERNAL_BYT_OBJS:.cmo=.cmx)

EXTERNAL_VOS =\
	coq_builtins.vo \
	wellfounded_externals.vo
#	weak_structures_externals.vo
#	random_foc_externals.vo array_foc_externals.vo

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
