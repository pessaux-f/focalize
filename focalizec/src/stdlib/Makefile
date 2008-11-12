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

# $Id: Makefile,v 1.62 2008-11-12 12:34:19 pessaux Exp $

ROOT_DIR = ..

include $(ROOT_DIR)/Makefile.config
include $(ROOT_DIR)/Makefile.install

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

FOCALIZE_SRC_UNSAFE =

FOCALIZE_FOS = $(FOCALIZE_SRC:.foc=.fo)
FOCALIZE_FOS_UNSAFE = $(FOCALIZE_SRC_UNSAFE:.foc=.fo)

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
	$(MKDIR) $(FOC_INSTALL_LIB)
	@if [ "$(FOCALIZE_FOS)" != "" ] ; then \
	  echo "Installing (FoCaLize) compiled certified libs." ; \
	  $(CP) $(FOCALIZE_FOS) $(FOC_INSTALL_LIB)/ ; \
	fi
	@if [ "$(FOCALIZE_FOS_UNSAFE)" != "" ] ; then \
	  echo "Installing (FoCaLize) compiled non-certified libs." ; \
	  $(CP) $(FOCALIZE_FOS_UNSAFE) $(FOC_INSTALL_LIB)/ ; \
	fi
	@if [ "$(FOCALIZE_BYT_OBJS)" != "" ] ; then \
	  echo "Installing (OCaml bytecode) compiled certified libs." ; \
	  $(CP) $(FOCALIZE_BYT_OBJS) $(FOC_INSTALL_LIB)/ ; \
	fi
	@if [ "$(FOCALIZE_BYT_OBJS_UNSAFE)" != "" ] ; then \
	  echo "Installing (OCaml bytecode) compiled non-certified libs." ; \
	  $(CP) $(FOCALIZE_BYT_OBJS_UNSAFE) $(FOC_INSTALL_LIB)/ ; \
	fi
	@if [ "$(FOCALIZE_BIN_OBJS)" != "" ] ; then \
	  echo "Installing (OCaml native) compiled certified libs." ; \
	  $(CP) $(FOCALIZE_BIN_OBJS) $(FOC_INSTALL_LIB)/ ; \
	fi
	@if [ "$(FOCALIZE_BIN_OBJS_UNSAFE)" != "" ] ; then \
	  echo "Installing (OCaml native) compiled non-certified libs." ; \
	  $(CP) $(FOCALIZE_BIN_OBJS_UNSAFE) $(FOC_INSTALL_LIB)/ ; \
	fi
	@if [ "$(FOCALIZE_VOS)" != "" ] ; then \
	  echo "Installing (Coq) compiled certified libs." ; \
	  $(CP) $(FOCALIZE_VOS) $(FOC_INSTALL_LIB)/ ; \
	fi
	@if [ "$(FOCALIZE_VOS_UNSAFE)" != "" ] ; then \
	  echo "Installing (Coq) compiled non-certified libs." ; \
	  $(CP) $(FOCALIZE_VOS_UNSAFE) $(FOC_INSTALL_LIB)/ ; \
	fi
	@if [ "$(EXTERNAL_BYT_OBJS)" != "" ] ; then \
	  echo "Installing (OCaml bytecode) external stubs." ; \
	  $(CP) $(EXTERNAL_BYT_OBJS) $(FOC_INSTALL_LIB)/ ; \
	fi
	@if [ "$(EXTERNAL_BIN_OBJS)" != "" ] ; then \
	  echo "Installing (OCaml native) external stubs." ; \
	  $(CP) $(EXTERNAL_BIN_OBJS) $(FOC_INSTALL_LIB)/ ; \
	fi
	@if [ "$(EXTERNAL_VOS)" != "" ] ; then \
	  echo "Installing (Coq) external stubs." ; \
	  $(CP) $(EXTERNAL_VOS) $(FOC_INSTALL_LIB)/ ; \
	fi

clean::
	$(RM) $(FOCALIZE_FOS) $(FOCALIZE_FOS_UNSAFE)
	$(RM) $(FOCALIZE_MLS) $(FOCALIZE_ZVS) $(FOCALIZE_VS)
	$(RM) $(FOCALIZE_MLS_UNSAFE) $(FOCALIZE_ZVS_UNSAFE) $(FOCALIZE_VS_UNSAFE)
	$(RM) $(FOCALIZE_BYT_OBJS) $(FOCALIZE_BYT_OBJS_UNSAFE)
	$(RM) $(FOCALIZE_BIN_OBJS) $(FOCALIZE_BIN_OBJS_UNSAFE)
	$(RM) $(FOCALIZE_VOS) $(FOCALIZE_VOS_UNSAFE)
	$(RM) $(EXTERNAL_BYT_OBJS) $(EXTERNAL_BIN_OBJS) $(EXTERNAL_VOS)
