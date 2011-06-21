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

# $Id: Makefile,v 1.4 2011-06-21 01:46:31 szekely Exp $

# Makefile for algebra structures implementations

LIB_INSTALL_SUB_DIR=security_policies

ROOT_DIR=../../..

include $(ROOT_DIR)/Makefile.config

PROJECT_LIB_INSTALL_DIR=$(LIB_DIR)/$(PROJECT_NAME_FULL)

LIB_INSTALL_DIR=$(PROJECT_LIB_INSTALL_DIR)/$(LIB_INSTALL_SUB_DIR)

STDLIB_DIR=$(ROOT_DIR)/src/stdlib

UTILSLIB_DIR=utils

POLICIESLIB_DIR=policies

EXEMPLESLIB_DIR=exemples

FOCALIZE_COMPILER=$(ROOT_DIR)/src/toplevel/focalizec

FOCALIZE_COMP_FLAGS=\
 -stop-before-coq -no-stdlib-path\
 -I $(STDLIB_DIR) -I $(UTILSLIB_DIR) -I $(POLICIESLIB_DIR) -I $(EXEMPLESLIB_DIR)

#From Makefile.config FOCALIZEC=$(FOCALIZE_COMPILER) $(FOCALIZE_COMP_FLAGS)

CAML_COMP_FLAGS=\
 -w A -warn-error Ae -dtypes\
 -I $(STDLIB_DIR) -I $(UTILSLIB_DIR) -I $(POLICIESLIB_DIR) -I $(EXEMPLESLIB_DIR)

ZVTOV_COMP_FLAGS=-script -zenon $(ZENON_COMPILER) -new

# From Makefile.config
# ZENON_LIB_DIR
COQ_INCLUDES=\
 -I $(ZENON_LIB_DIR)\
 -I $(STDLIB_DIR) -I $(UTILSLIB_DIR) -I $(POLICIESLIB_DIR) -I $(EXEMPLESLIB_DIR)

FOCALIZE_SRC=\
 utils/finite_sets.fcl\
 utils/lts_mutable.fcl\
 utils/lts.fcl\
 utils/relations_mutable.fcl\
 utils/relations.fcl\
 policies/p_policies.fcl\
 policies/secure_system.fcl\
 exemples/accesses.fcl\
 exemples/accesses_system.fcl\
 exemples/accesses_p_policies.fcl\
 exemples/hru.fcl

# FOCALIZE_SRC_UNSAFE are compiled not using the implicit rules. They have
# their own rules at the end of this Makefile.
FOCALIZE_SRC_UNSAFE=

FOCALIZE_FOS=$(FOCALIZE_SRC:.fcl=.fo)
FOCALIZE_FOS_UNSAFE=$(FOCALIZE_SRC_UNSAFE:.fcl=.fo)

FOCALIZE_MLS=$(FOCALIZE_SRC:.fcl=.ml)
FOCALIZE_MLS_UNSAFE=$(FOCALIZE_SRC_UNSAFE:.fcl=.ml)

FOCALIZE_CMIS=$(FOCALIZE_SRC:.fcl=.cmi)
FOCALIZE_CMIS_UNSAFE=$(FOCALIZE_SRC_UNSAFE:.fcl=.cmi)

FOCALIZE_BYT_OBJS=$(FOCALIZE_MLS:.ml=.cmo)
FOCALIZE_BYT_OBJS_UNSAFE=$(FOCALIZE_MLS_UNSAFE:.ml=.cmo)

FOCALIZE_BIN_OBJS=$(FOCALIZE_BYT_OBJS:.cmo=.cmx)
FOCALIZE_BIN_OBJS_UNSAFE=$(FOCALIZE_BYT_OBJS_UNSAFE:.cmo=.cmx)

FOCALIZE_ZVS=$(FOCALIZE_SRC:.fcl=.zv)
FOCALIZE_ZVS_UNSAFE=$(FOCALIZE_SRC_UNSAFE:.fcl=.zv)

FOCALIZE_VS=$(FOCALIZE_ZVS:.zv=.v)
FOCALIZE_VS_UNSAFE=$(FOCALIZE_SRC_UNSAFE:.fcl=.v)

FOCALIZE_VOS=$(FOCALIZE_VS:.v=.vo)
FOCALIZE_VOS_UNSAFE=$(FOCALIZE_SRC_UNSAFE:.fcl=.vo)

FOCALIZE_DOCS=$(FOCALIZE_SRC:.fcl=.fcd)
FOCALIZE_DOCS_UNSAFE=$(FOCALIZE_SRC_UNSAFE:.fcl=.fcd)

FOCALIZE_MML=$(FOCALIZE_DOCS:.fcd=.xml)
FOCALIZE_MML_UNSAFE=$(FOCALIZE_DOCS_UNSAFE:.fcd=.xml)

FOCALIZE_O=$(FOCALIZE_SRC:.fcl=.o)
FOCALIZE_O_UNSAFE=$(FOCALIZE_SRC_UNASAFE:.fcl=.o)

FOCALIZE_PFC=$(FOCALIZE_SRC:.fcl=.pfc)
FOCALIZE_PFC_UNSAFE=$(FOCALIZE_SRC_UNASAFE:.fcl=.pfc)

FOCALIZE_ANNOT=$(FOCALIZE_SRC:.fcl=.annot)
FOCALIZE_ANNOT_UNSAFE=$(FOCALIZE_SRC_UNASAFE:.fcl=.annot)

EXTERNAL_BYT_OBJS=
EXTERNAL_CMIS=$(EXTERNAL_BYT_OBJS:.cmo=.cmi)

EXTERNAL_BIN_OBJS=$(EXTERNAL_BYT_OBJS:.cmo=.cmx)

EXTERNAL_VOS=

STD_BIN_OBJS=\
 ml_builtins.cmx\
 basics.cmx\
 sets_externals.cmx\
 sets.cmx\
 products.cmx

FINITE_SETS_DEPS=utils/finite_sets.cmx
LTS_MUTABLE_DEPS=utils/lts_mutable.cmx
LTS_DEPS=utils/lts.cmx
RELATIONS_MUTABLE_DEPS=utils/finite_sets.cmx utils/relations_mutable.cmx
RELATIONS_DEPS=utils/finite_sets.cmx utils/relations.cmx
P_POLICIES_DEPS=utils/finite_sets.cmx utils/relations.cmx\
 policies/p_policies.cmx
SECURE_SYSTEM_DEPS=$(P_POLICIES_DEPS) utils/lts.cmx policies/secure_system.cmx
ACCESSES_DEPS=utils/finite_sets.cmx exemples/accesses.cmx
ACCESSES_SYSTEM_DEPS=$(ACCESSES_DEPS) utils/lts.cmx exemples/accesses_system.cmx
ACCESSES_P_POLYCIES_DEPS=$(P_POLICIES_DEPS) exemples/accesses.cmx\
 exemples/accesses_p_policies.cmx
HRU_DEPS=$(ACCESSES_P_POLYCIES_DEPS) utils/lts.cmx exemples/accesses_system.cmx\
 policies/secure_system.cmx exemples/hru.cmx

CAML_COMPILE_CMD=$(CAML_BIN_COMPILER) -o $@ \
                   -I $(STDLIB_DIR) -I $(UTILSLIB_DIR)\
                   -I $(POLICIESLIB_DIR) -I $(EXEMPLESLIB_DIR)\
                   $(STD_BIN_OBJS) $(EXTERNAL_BIN_OBJS)

BIN_EXE = $(FOCALIZE_SRC:.fcl=.test)

include $(ROOT_DIR)/Makefile.common

all: $(BIN_EXE)

lib: all

focalizedoc:\
 $(FOCALIZE_MML) $(FOCALIZE_MML_UNSAFE)

byt:\
 $(EXTERNAL_BYT_OBJS) $(EXTERNAL_VOS)\
 $(FOCALIZE_MLS) $(FOCALIZE_MLS_UNSAFE)\
 $(FOCALIZE_ZVS)\
 $(FOCALIZE_BYT_OBJS) $(FOCALIZE_BYT_OBJS_UNSAFE)\
 $(FOCALIZE_VS) $(FOCALIZE_VOS)

bin:\
 $(EXTERNAL_BIN_OBJS) $(EXTERNAL_VOS)\
 $(FOCALIZE_MLS) $(FOCALIZE_MLS_UNSAFE)\
 $(FOCALIZE_ZVS)\
 $(FOCALIZE_BIN_OBJS) $(FOCALIZE_BIN_OBJS_UNSAFE)\
 $(FOCALIZE_VS) $(FOCALIZE_VOS)

utils/finite_sets.test: byt bin
	$(CAML_COMPILE_CMD) $(FINITE_SETS_DEPS)

utils/lts_mutable.test: byt bin
	$(CAML_COMPILE_CMD) $(LTS_MUTABLE_DEPS)

utils/lts.test: byt bin
	$(CAML_COMPILE_CMD) $(LTS_DEPS)

utils/relations_mutable.test: byt bin
	$(CAML_COMPILE_CMD) $(RELATIONS_MUTABLE_DEPS)

utils/relations.test: byt bin
	$(CAML_COMPILE_CMD) $(RELATIONS_DEPS)

policies/p_policies.test: byt bin
	$(CAML_COMPILE_CMD) $(P_POLICIES_DEPS)

policies/secure_system.test: byt bin
	$(CAML_COMPILE_CMD) $(SECURE_SYSTEM_DEPS)

exemples/accesses.test: byt bin
	$(CAML_COMPILE_CMD) $(ACCESSES_DEPS)

exemples/accesses_system.test: byt bin
	$(CAML_COMPILE_CMD) $(ACCESSES_SYSTEM_DEPS)

exemples/accesses_p_policies.test: byt bin
	$(CAML_COMPILE_CMD) $(ACCESSES_P_POLYCIES_DEPS)

exemples/hru.test: byt bin
	$(CAML_COMPILE_CMD) $(HRU_DEPS)

install:
	@$(MKDIR) $(LIB_INSTALL_DIR) &&\
	if [ "$(FOCALIZE_FOS)" != "" ] ; then\
	  echo "Installing (FoCaLize) compiled certified libs." &&\
	  $(CP) $(FOCALIZE_FOS) $(LIB_INSTALL_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_FOS_UNSAFE)" != "" ] ; then\
	  echo "Installing (FoCaLize) compiled non-certified libs." &&\
	  $(CP) $(FOCALIZE_FOS_UNSAFE) $(LIB_INSTALL_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_CMIS)" != "" ] ; then\
	  echo "Installing (Ocaml interface) compiled certified libs." &&\
	  $(CP) $(FOCALIZE_CMIS) $(LIB_INSTALL_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_CMIS_UNSAFE)" != "" ] ; then\
	  echo "Installing (Ocaml interface) compiled non-certified libs." &&\
	  $(CP) $(FOCALIZE_CMIS_UNSAFE) $(LIB_INSTALL_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_BYT_OBJS)" != "" ] ; then\
	  $(CP) $(FOCALIZE_BYT_OBJS) $(LIB_INSTALL_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_BYT_OBJS_UNSAFE)" != "" ] ; then\
	  $(CP) $(FOCALIZE_BYT_OBJS_UNSAFE) $(LIB_INSTALL_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_BIN_OBJS)" != "" ] ; then\
	  $(CP) $(FOCALIZE_BIN_OBJS) $(LIB_INSTALL_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_BIN_OBJS_UNSAFE)" != "" ] ; then\
	  $(CP) $(FOCALIZE_BIN_OBJS_UNSAFE) $(LIB_INSTALL_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_VOS)" != "" ] ; then\
	  $(CP) $(FOCALIZE_VOS) $(LIB_INSTALL_DIR)/;\
	fi &&\
	if [ "$(EXTERNAL_CMIS)" != "" ] ; then\
	  $(CP) $(EXTERNAL_CMIS) $(LIB_INSTALL_DIR)/;\
	fi &&\
	if [ "$(EXTERNAL_BYT_OBJS)" != "" ] ; then\
	  $(CP) $(EXTERNAL_BYT_OBJS) $(LIB_INSTALL_DIR)/;\
	fi &&\
	if [ "$(EXTERNAL_BIN_OBJS)" != "" ] ; then\
	  $(CP) $(EXTERNAL_BIN_OBJS) $(LIB_INSTALL_DIR)/;\
	fi &&\
	if [ "$(EXTERNAL_VOS)" != "" ] ; then\
	  $(CP) $(EXTERNAL_VOS) $(LIB_INSTALL_DIR)/;\
	fi

uninstall-local:
	$(RM) $(LIB_INSTALL_DIR)

clean-local:
	$(RM) $(FOCALIZE_MLS) $(FOCALIZE_ZVS) $(FOCALIZE_VS)
	$(RM) $(FOCALIZE_MLS_UNSAFE) $(FOCALIZE_ZVS_UNSAFE) $(FOCALIZE_VS_UNSAFE)
	$(RM) $(FOCALIZE_FOS) $(FOCALIZE_CMIS) $(FOCALIZE_BYT_OBJS)
	$(RM) $(FOCALIZE_BIN_OBJS) $(FOCALIZE_VOS) $(FOCALIZE_O) $(FOCALIZE_PFC)
	$(RM) $(FOCALIZE_ANNOT)
	$(RM) $(FOCALIZE_FOS_UNSAFE) $(FOCALIZE_CMIS_UNSAFE)
	$(RM) $(FOCALIZE_BYT_OBJS_UNSAFE) $(FOCALIZE_BIN_OBJS_UNSAFE)
	$(RM) $(FOCALIZE_VOS_UNSAFE) $(FOCALIZE_O_UNSAFE) $(FOCALIZE_PFC_UNSAFE)
	$(RM) $(FOCALIZE_ANNOT_UNSAFE)
	$(RM) $(BIN_EXE)