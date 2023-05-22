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

# $Id: Makefile,v 1.3 2012-10-15 14:56:45 pessaux Exp $

LIB_INSTALL_SUB_DIR = airport_security

ROOT_DIR = ../../../..

PROJECT_INSTALL_LIB_DIR = $(LIB_DIR)/$(PROJECT_NAME_FULL)

INSTALL_LIB_DIR = $(PROJECT_INSTALL_LIB_DIR)/$(LIB_INSTALL_SUB_DIR)

STDLIB_DIR = $(ROOT_DIR)/focalizec/src/stdlib

FOCALIZEC = $(ROOT_DIR)/focalizec/src/toplevel/focalizec

FOCALIZE_COMP_FLAGS = -no-stdlib-path -I $(STDLIB_DIR)

CAML_FLAGS = -w A -warn-error $(WARN_ERROR) -dtypes -I $(STDLIB_DIR)

#From Makefile.config ZENON_LIB_DIR
COQ_INCLUDES = -Q $(ZENON_LIB_DIR) "" -Q $(STDLIB_DIR) ""

include $(ROOT_DIR)/Makefile.common

FOCALIZE_SRC = basic_sets.fcl a17subjects.fcl conversionFunctions.fcl \
               airsideSubjects.fcl aircraftEnv.fcl ordinaryPassengersEnv.fcl \
               specialPassengersEnv.fcl holdBaggageEnv.fcl a17property4_2.fcl \
               a17property4_3.fcl a17property4_7.fcl

# FOCALIZE_SRC_UNSAFE are compiled not using the implicit rules. They have
# their own rules at the end of this Makefile.
FOCALIZE_SRC_UNSAFE =

FOCALIZE_FOS = $(FOCALIZE_SRC:.fcl=.fo)
FOCALIZE_FOS_UNSAFE = $(FOCALIZE_SRC_UNSAFE:.fcl=.fo)

FOCALIZE_MLS = $(FOCALIZE_SRC:.fcl=.ml)
FOCALIZE_MLS_UNSAFE = $(FOCALIZE_SRC_UNSAFE:.fcl=.ml)

FOCALIZE_CMIS = $(FOCALIZE_MLS:.ml=.cmi)
FOCALIZE_CMIS_UNSAFE = $(FOCALIZE_MLS_UNSAFE:.ml=.cmi)

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

lib: all

byt: \
      $(FOCALIZE_MLS) $(FOCALIZE_MLS_UNSAFE)\
      $(FOCALIZE_ZVS)\
      $(FOCALIZE_BYT_OBJS) $(FOCALIZE_BYT_OBJS_UNSAFE)\
      $(FOCALIZE_VS) $(FOCALIZE_VOS)

bin: \
      $(FOCALIZE_MLS) $(FOCALIZE_MLS_UNSAFE)\
      $(FOCALIZE_ZVS)\
      $(FOCALIZE_BIN_OBJS) $(FOCALIZE_BIN_OBJS_UNSAFE)\
      $(FOCALIZE_VS) $(FOCALIZE_VOS)

install:
	@$(MKDIR) $(INSTALL_LIB_DIR) && \
	if [ "$(FOCALIZE_FOS)" != "" ] ; then \
	  echo "Installing (FoCaLize) compiled certified libs." && \
	  $(CP) $(FOCALIZE_FOS) $(INSTALL_LIB_DIR)/; \
	fi && \
	if [ "$(FOCALIZE_FOS_UNSAFE)" != "" ] ; then \
	  echo "Installing (FoCaLize) compiled non-certified libs." && \
	  $(CP) $(FOCALIZE_FOS_UNSAFE) $(INSTALL_LIB_DIR)/; \
	fi && \
	if [ "$(FOCALIZE_CMIS)" != "" ] ; then \
	  echo "Installing (Ocaml interface) compiled certified libs." && \
	  $(CP) $(FOCALIZE_CMIS) $(INSTALL_LIB_DIR)/ ; \
	fi && \
	if [ "$(FOCALIZE_CMIS_UNSAFE)" != "" ] ; then \
	  echo "Installing (Ocaml interface) compiled non-certified libs." && \
	  $(CP) $(FOCALIZE_CMIS_UNSAFE) $(INSTALL_LIB_DIR)/ ; \
	fi && \
	if [ "$(FOCALIZE_BYT_OBJS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BYT_OBJS) $(INSTALL_LIB_DIR)/; \
	fi && \
	if [ "$(FOCALIZE_BYT_OBJS_UNSAFE)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BYT_OBJS_UNSAFE) $(INSTALL_LIB_DIR)/; \
	fi && \
	if [ "$(FOCALIZE_BIN_OBJS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BIN_OBJS) $(INSTALL_LIB_DIR)/; \
	fi && \
	if [ "$(FOCALIZE_BIN_OBJS_UNSAFE)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BIN_OBJS_UNSAFE) $(INSTALL_LIB_DIR)/; \
	fi && \
	if [ "$(FOCALIZE_VOS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_VOS) $(INSTALL_LIB_DIR)/; \
	fi

clean:
	$(RM) *.fo *.ml *.cm* *.zv *.v *.vo *.o *.pfc *.annot
