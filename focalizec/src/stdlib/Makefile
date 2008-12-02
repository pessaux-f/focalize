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

# $Id: Makefile,v 1.69 2008-12-02 13:58:28 rr Exp $

ROOT_DIR = ..

include $(ROOT_DIR)/Makefile.config

FOCALIZE_COMPILER = $(ROOT_DIR)/toplevel/focalizec

FOCALIZE_COMP_FLAGS = -no-stdlib-path

LIB_INSTALL_DIR = $(LIB_DIR)/focalize

#From Makefile.config FOCALIZEC = $(FOCALIZE_COMPILER) $(FOCALIZE_COMP_FLAGS)

COQ_INCLUDES = -I ../../zenon/

FOCALIZE_SRC = basics.fcl \
	sets.fcl\
	products.fcl \
	lattices.fcl\
	orders.fcl \
	orders_and_lattices.fcl \
	sets_orders.fcl \
	wellfounded.fcl \
	sums.fcl

FOCALIZE_SRC_UNSAFE =

FOCALIZE_FOS = $(FOCALIZE_SRC:.fcl=.fo)
FOCALIZE_FOS_UNSAFE = $(FOCALIZE_SRC_UNSAFE:.fcl=.fo)

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

install::
	$(MKDIR) $(LIB_INSTALL_DIR)
	@if [ "$(FOCALIZE_FOS)" != "" ] ; then \
	  echo "Installing (FoCaLize) compiled certified libs." ; \
	  $(CP) $(FOCALIZE_FOS) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(FOCALIZE_FOS_UNSAFE)" != "" ] ; then \
	  echo "Installing (FoCaLize) compiled non-certified libs." ; \
	  $(CP) $(FOCALIZE_FOS_UNSAFE) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(FOCALIZE_BYT_OBJS)" != "" ] ; then \
	  echo "Installing (OCaml bytecode) compiled certified libs." ; \
	  $(CP) $(FOCALIZE_BYT_OBJS) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(FOCALIZE_BYT_OBJS_UNSAFE)" != "" ] ; then \
	  echo "Installing (OCaml bytecode) compiled non-certified libs." ; \
	  $(CP) $(FOCALIZE_BYT_OBJS_UNSAFE) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(FOCALIZE_BIN_OBJS)" != "" ] ; then \
	  echo "Installing (OCaml native) compiled certified libs." ; \
	  $(CP) $(FOCALIZE_BIN_OBJS) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(FOCALIZE_BIN_OBJS_UNSAFE)" != "" ] ; then \
	  echo "Installing (OCaml native) compiled non-certified libs." ; \
	  $(CP) $(FOCALIZE_BIN_OBJS_UNSAFE) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(FOCALIZE_VOS)" != "" ] ; then \
	  echo "Installing (Coq) compiled certified libs." ; \
	  $(CP) $(FOCALIZE_VOS) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(FOCALIZE_VOS_UNSAFE)" != "" ] ; then \
	  echo "Installing (Coq) compiled non-certified libs." ; \
	  $(CP) $(FOCALIZE_VOS_UNSAFE) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(EXTERNAL_BYT_OBJS)" != "" ] ; then \
	  echo "Installing (OCaml bytecode) external stubs." ; \
	  $(CP) $(EXTERNAL_BYT_OBJS) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(EXTERNAL_BIN_OBJS)" != "" ] ; then \
	  echo "Installing (OCaml native) external stubs." ; \
	  $(CP) $(EXTERNAL_BIN_OBJS) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(EXTERNAL_VOS)" != "" ] ; then \
	  echo "Installing (Coq) external stubs." ; \
	  $(CP) $(EXTERNAL_VOS) $(LIB_INSTALL_DIR)/ ; \
	fi

clean::
	$(RM) $(FOCALIZE_FOS) $(FOCALIZE_FOS_UNSAFE)
	$(RM) $(FOCALIZE_MLS) $(FOCALIZE_ZVS) $(FOCALIZE_VS)
	$(RM) $(FOCALIZE_MLS_UNSAFE) $(FOCALIZE_ZVS_UNSAFE) $(FOCALIZE_VS_UNSAFE)
	$(RM) $(FOCALIZE_BYT_OBJS) $(FOCALIZE_BYT_OBJS_UNSAFE)
	$(RM) $(FOCALIZE_BIN_OBJS) $(FOCALIZE_BIN_OBJS_UNSAFE)
	$(RM) $(FOCALIZE_VOS) $(FOCALIZE_VOS_UNSAFE)
	$(RM) $(EXTERNAL_BYT_OBJS) $(EXTERNAL_BIN_OBJS) $(EXTERNAL_VOS)


depend::

#depend:: $(FOCALIZE_SRC)
#	focalizedep $@ >>.depend

#depend:: $(FOCALIZE_VS)
#	coqdep $@ >>.depend
