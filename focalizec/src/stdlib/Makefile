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

# $Id: Makefile,v 1.49 2008-10-14 12:13:21 pessaux Exp $

ROOT_DIR = ..

include $(ROOT_DIR)/Makefile.config

# Just redefine to remove the warnings because FoCaL has some warnings
# in its generated code. Especially "Warning Z: unused variable ..."
# because FoCaL doesn't have the "_" ident which would be usefull in
# the case of let foo (x in unit) = ...
CAMLBYT = ocamlc
CAMLBIN = ocamlc

BYT_FOCAL_COMPILER = $(ROOT_DIR)/toplevel/focalizec.byt
BIN_FOCAL_COMPILER = $(BYT_FOCAL_COMPILER:.byt=.bin)

FOCAL_COMPILER = $(BIN_FOCAL_COMPILER)

FOCAL_COMP_FLAGS = -no-stdlib-path

#From Makefile.config FOCALC = $(FOCAL_COMPILER) $(FOCAL_COMP_FLAGS)

COQ_INCLUDES = -I ../../zenon/

FOCAL_SRC = basics.foc \
	sets.foc\
	sets_orders.foc \
	wellfounded.foc #\
#	products.foc

FOCAL_SRC_UNSAFE = \
	ensembles_finis.foc

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
  ml_builtins.cmo sets_externals.cmo sets_orders_externals.cmo
#  weak_structures_externals.cmo
#  random_foc_externals.cmo

EXTERNAL_BINOBJS = $(EXTERNAL_BYTOBJS:.cmo=.cmx)

EXTERNAL_VOS =\
	coq_builtins.vo \
	wellfounded_externals.vo
#	weak_structures_externals.vo
#	random_foc_externals.vo array_foc_externals.vo

include $(ROOT_DIR)/Makefile.common

lib: all

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

$(BYT_FOCAL_COMPILER) $(BIN_FOCAL_COMPILER):
	cd $(ROOT_DIR); $(MAKE) all
