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

# $Id: Makefile,v 1.8 2012-10-15 14:59:24 pessaux Exp $

# Makefile for algebra structures implementations

ROOT_DIR=../../../..

LIB_INSTALL_DIR=$(LIB_DIR)/$(PROJECT_NAME_FULL)

FOCALIZE_COMPILER=$(ROOT_DIR)/src/toplevel/focalizec

STDLIB_DIR=../../stdlib

FOCALIZEC=../../toplevel/focalizec

FOCALIZE_COMP_FLAGS=-no-stdlib-path -I $(STDLIB_DIR) $(FOCALIZEC_COQ_COMPATIBILITY_OPTION)

CAML_FLAGS=-w A -warn-error $(WARN_ERROR) -dtypes
CAML_INCLUDES=-I $(STDLIB_DIR)

ZENON_LIB_DIR=../../../../zenon
COQ_INCLUDES=-I $(ZENON_LIB_DIR) -I $(STDLIB_DIR)

include $(ROOT_DIR)/Makefile.common
include $(ROOT_DIR)/focalizec/.install_var
include $(ROOT_DIR)/focalizec/.config_var

FOCALIZE_SRC=\
 pair.fcl\
 peano.fcl

FOCALIZE_FOS=$(FOCALIZE_SRC:.fcl=.fo)
FOCALIZE_MLS=$(FOCALIZE_SRC:.fcl=.ml)

FOCALIZE_CMIS=$(FOCALIZE_SRC:.fcl=.cmi)

FOCALIZE_BYT_OBJS=$(FOCALIZE_MLS:.ml=.cmo)
FOCALIZE_BIN_OBJS=$(FOCALIZE_BYT_OBJS:.cmo=.cmx)

FOCALIZE_ZVS=$(FOCALIZE_SRC:.fcl=.zv)
FOCALIZE_VS=$(FOCALIZE_ZVS:.zv=.v)
FOCALIZE_VOS=$(FOCALIZE_VS:.v=.vo)


lib: all

byt:\
 $(FOCALIZE_MLS)\
 $(FOCALIZE_ZVS)\
 $(FOCALIZE_BYT_OBJS)\
 $(FOCALIZE_VS) $(FOCALIZE_VOS)

bin:\
 $(EXTERNAL_BIN_OBJS) $(EXTERNAL_VOS)\
 $(FOCALIZE_MLS)\
 $(FOCALIZE_ZVS)\
 $(FOCALIZE_BIN_OBJS)\
 $(FOCALIZE_VS) $(FOCALIZE_VOS)

install:
	@$(MKDIR) $(INSTALL_LIB_DIR) &&\
	if [ "$(FOCALIZE_FOS)" != "" ] ; then\
	  echo "Installing (FoCaLize) compiled certified libs." &&\
	  $(CP) $(FOCALIZE_FOS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_CMIS)" != "" ] ; then\
	  echo "Installing (OCaml interface) compiled certified libs." &&\
	  $(CP) $(FOCALIZE_CMIS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_BYT_OBJS)" != "" ] ; then\
	  echo "Installing (OCaml bytecode) compiled certified libs." &&\
	  $(CP) $(FOCALIZE_BYT_OBJS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_BIN_OBJS)" != "" ] ; then\
	  echo "Installing (OCaml native) compiled certified libs." &&\
	  $(CP) $(FOCALIZE_BIN_OBJS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_VOS)" != "" ] ; then\
	  echo "Installing (Coq) compiled certified libs." &&\
	  $(CP) $(FOCALIZE_VOS) $(INSTALL_LIB_DIR)/;\
	fi

clean:
	$(RM) $(FOCALIZE_FOS) $(FOCALIZE_MLS) $(FOCALIZE_ZVS) $(FOCALIZE_VS)
	$(RM) $(FOCALIZE_BYT_OBJS) $(FOCALIZE_BIN_OBJS) $(FOCALIZE_VOS)
	$(RM) $(FOCALIZE_CMIS) *.annot *.glob *.fcd
	$(RM) *.cm* *.o *.a *.annot *.out *.output *.bin *.byt

# Currently not handled.
depend:
