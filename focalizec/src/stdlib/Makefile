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

# $Id: Makefile,v 1.84 2009-03-10 15:12:53 rr Exp $

# Makefile for the FoCaLize standard library

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

LIB_INSTALL_DIR=$(LIB_DIR)

FOCALIZE_COMPILER = $(ROOT_DIR)/src/toplevel/focalizec

FOCALIZE_COMP_FLAGS = -no-stdlib-path

# From Makefile.config FOCALIZEC = $(FOCALIZE_COMPILER) $(FOCALIZE_COMP_FLAGS)
# COQ_INCLUDES = -I $(ZENON_LIB_DIR)

FOCALIZE_SRC = basics.fcl\
	sets.fcl\
	products.fcl\
	lattices.fcl\
	orders.fcl\
	strict_orders.fcl\
	orders_and_lattices.fcl\
	sets_orders.fcl\
	wellfounded.fcl\
	sums.fcl\
	quotients.fcl

FOCALIZE_FOS = $(FOCALIZE_SRC:.fcl=.fo)
FOCALIZE_MLS = $(FOCALIZE_SRC:.fcl=.ml)

FOCALIZE_CMIS = $(FOCALIZE_SRC:.fcl=.cmi)

FOCALIZE_BYT_OBJS = $(FOCALIZE_MLS:.ml=.cmo)
FOCALIZE_BIN_OBJS = $(FOCALIZE_BYT_OBJS:.cmo=.cmx)

FOCALIZE_ZVS = $(FOCALIZE_SRC:.fcl=.zv)
FOCALIZE_VS = $(FOCALIZE_ZVS:.zv=.v)
FOCALIZE_VOS = $(FOCALIZE_VS:.v=.vo)

FOCALIZE_DOCS = $(FOCALIZE_SRC:.fcl=.fcd)
FOCALIZE_DOCS_UNSAFE = $(FOCALIZE_SRC_UNSAFE:.fcl=.fcd)

FOCALIZE_MML = $(FOCALIZE_DOCS:.fcd=.xml)
FOCALIZE_MML_UNSAFE = $(FOCALIZE_DOCS_UNSAFE:.fcd=.xml)

EXTERNAL_BYT_OBJS =\
  ml_builtins.cmo sets_externals.cmo sets_orders_externals.cmo
#  weak_structures_externals.cmo
#  random_foc_externals.cmo

EXTERNAL_BIN_OBJS = $(EXTERNAL_BYT_OBJS:.cmo=.cmx)

EXTERNAL_VOS =\
	coq_builtins.vo\
	wellfounded_externals.vo
#	weak_structures_externals.vo
#	random_foc_externals.vo array_foc_externals.vo

include $(ROOT_DIR)/Makefile.common

lib: all

focalizedoc:: \
	$(FOCALIZE_MML) $(FOCALIZE_MML_UNSAFE)

byt:: \
      $(EXTERNAL_BYT_OBJS) $(EXTERNAL_VOS)\
      $(FOCALIZE_MLS)\
      $(FOCALIZE_ZVS)\
      $(FOCALIZE_BYT_OBJS)\
      $(FOCALIZE_VS) $(FOCALIZE_VOS)

bin:: \
      $(EXTERNAL_BIN_OBJS) $(EXTERNAL_VOS)\
      $(FOCALIZE_MLS)\
      $(FOCALIZE_ZVS)\
      $(FOCALIZE_BIN_OBJS)\
      $(FOCALIZE_VS) $(FOCALIZE_VOS)

install::
	@$(MKDIR) $(LIB_INSTALL_DIR) && \
	if [ "$(FOCALIZE_FOS)" != "" ]; then \
	  echo "Installing (FoCaLize) compiled certified libs." && \
	  $(CP) $(FOCALIZE_FOS) $(LIB_INSTALL_DIR)/; \
	fi && \
	if [ "$(FOCALIZE_CMIS)" != "" ]; then \
	  echo "Installing (OCaml interface) compiled certified libs." && \
	  $(CP) $(FOCALIZE_CMIS) $(LIB_INSTALL_DIR)/; \
	fi && \
	if [ "$(FOCALIZE_BYT_OBJS)" != "" ]; then \
	  echo "Installing (OCaml bytecode) compiled certified libs." && \
	  $(CP) $(FOCALIZE_BYT_OBJS) $(LIB_INSTALL_DIR)/; \
	fi && \
	if [ "$(FOCALIZE_BIN_OBJS)" != "" ]; then \
	  echo "Installing (OCaml native) compiled certified libs." && \
	  $(CP) $(FOCALIZE_BIN_OBJS) $(LIB_INSTALL_DIR)/; \
	fi && \
	if [ "$(FOCALIZE_VOS)" != "" ]; then \
	  echo "Installing (Coq) compiled certified libs." && \
	  $(CP) $(FOCALIZE_VOS) $(LIB_INSTALL_DIR)/; \
	fi && \
	if [ "$(EXTERNAL_BYT_OBJS)" != "" ]; then \
	  echo "Installing (OCaml bytecode) external stubs." && \
	  $(CP) $(EXTERNAL_BYT_OBJS) $(LIB_INSTALL_DIR)/; \
	fi && \
	if [ "$(EXTERNAL_BIN_OBJS)" != "" ]; then \
	  echo "Installing (OCaml native) external stubs." && \
	  $(CP) $(EXTERNAL_BIN_OBJS) $(LIB_INSTALL_DIR)/; \
	fi && \
	if [ "$(EXTERNAL_VOS)" != "" ]; then \
	  echo "Installing (Coq) external stubs." && \
	  $(CP) $(EXTERNAL_VOS) $(LIB_INSTALL_DIR)/; \
	fi

clean::
	$(RM) $(FOCALIZE_FOS) $(FOCALIZE_MLS) $(FOCALIZE_ZVS) $(FOCALIZE_VS) && \
	$(RM) $(FOCALIZE_BYT_OBJS) $(FOCALIZE_BIN_OBJS) $(FOCALIZE_VOS) && \
	$(RM) $(EXTERNAL_BYT_OBJS) $(EXTERNAL_BIN_OBJS) $(EXTERNAL_VOS) && \
	$(RM) $(FOCALIZE_CMIS) *.annot *.glob *.fcd


depend::

#depend:: $(FOCALIZE_VS)
#	coqdep $@ >>.depend
