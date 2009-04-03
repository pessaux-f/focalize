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

# $Id: Makefile,v 1.6 2009-04-03 15:50:36 doligez Exp $

# Makefile for algebra structures implementations

ROOT_DIR = ../../..

include $(ROOT_DIR)/Makefile.config

LIB_INSTALL_DIR = $(LIB_DIR)/$(PROJECT_NAME_FULL)

FOCALIZE_COMPILER = $(ROOT_DIR)/src/toplevel/focalizec

STDLIB_DIR = $(ROOT_DIR)/src/stdlib

FOCALIZE_COMP_FLAGS = -no-stdlib-path -I $(STDLIB_DIR)

#From Makefile.config FOCALIZEC = $(FOCALIZE_COMPILER) $(FOCALIZE_COMP_FLAGS)

CAML_COMP_FLAGS = -w A -warn-error Ae -dtypes -I $(STDLIB_DIR)

# From Makefile.config ZENON_LIB_DIR
# Was ZENON_LIB_DIR = $(ROOT_DIR)/../zenon/
COQ_INCLUDES = -I $(ZENON_LIB_DIR) -I $(STDLIB_DIR)

FOCALIZE_SRC =\
  pair.fcl \
  peano.fcl

FOCALIZE_FOS = $(FOCALIZE_SRC:.fcl=.fo)
FOCALIZE_MLS = $(FOCALIZE_SRC:.fcl=.ml)

FOCALIZE_CMIS = $(FOCALIZE_SRC:.fcl=.cmi)

FOCALIZE_BYT_OBJS = $(FOCALIZE_MLS:.ml=.cmo)
FOCALIZE_BIN_OBJS = $(FOCALIZE_BYT_OBJS:.cmo=.cmx)

FOCALIZE_ZVS = $(FOCALIZE_SRC:.fcl=.zv)
FOCALIZE_VS = $(FOCALIZE_ZVS:.zv=.v)
FOCALIZE_VOS = $(FOCALIZE_VS:.v=.vo)

include $(ROOT_DIR)/Makefile.common

lib: all

byt:: \
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
	if [ "$(FOCALIZE_FOS)" != "" ] ; then \
	  echo "Installing (FoCaLize) compiled certified libs." && \
	  $(CP) $(FOCALIZE_FOS) $(LIB_INSTALL_DIR)/; \
	fi && \
	if [ "$(FOCALIZE_CMIS)" != "" ] ; then \
	  echo "Installing (OCaml interface) compiled certified libs." && \
	  $(CP) $(FOCALIZE_CMIS) $(LIB_INSTALL_DIR)/; \
	fi && \
	if [ "$(FOCALIZE_BYT_OBJS)" != "" ] ; then \
	  echo "Installing (OCaml bytecode) compiled certified libs." && \
	  $(CP) $(FOCALIZE_BYT_OBJS) $(LIB_INSTALL_DIR)/; \
	fi && \
	if [ "$(FOCALIZE_BIN_OBJS)" != "" ] ; then \
	  echo "Installing (OCaml native) compiled certified libs." && \
	  $(CP) $(FOCALIZE_BIN_OBJS) $(LIB_INSTALL_DIR)/; \
	fi && \
	if [ "$(FOCALIZE_VOS)" != "" ] ; then \
	  echo "Installing (Coq) compiled certified libs." && \
	  $(CP) $(FOCALIZE_VOS) $(LIB_INSTALL_DIR)/; \
	fi

uninstall::
	@($(CD) $(LIB_DIR) && \
	  $(RM) $(LIB_INSTALL_DIR))

clean::
	@$(RM) $(FOCALIZE_FOS) $(FOCALIZE_MLS) $(FOCALIZE_ZVS) $(FOCALIZE_VS) && \
	$(RM) $(FOCALIZE_BYT_OBJS) $(FOCALIZE_BIN_OBJS) $(FOCALIZE_VOS) && \
	$(RM) $(FOCALIZE_CMIS) *.annot *.glob *.fcd
