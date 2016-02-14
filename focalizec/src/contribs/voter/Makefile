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


# Makefile for algebra structures implementations

ROOT_DIR=../../../..

LIB_INSTALL_DIR=$(LIB_DIR)/voter

STDLIB_DIR=../../stdlib

ALGEBRALIB_DIR=../../extlib/algebra

UTILSLIB_DIR=../utils

FOCALIZEC=../../toplevel/focalizec
FOCALIZEDEP=../../focalizedep/focalizedep

FOCALIZE_COMP_FLAGS=\
 -no-stdlib-path -I $(STDLIB_DIR) -I $(ALGEBRALIB_DIR)\
 -I $(UTILSLIB_DIR) $(FOCALIZEC_COQ_COMPATIBILITY_OPTION)

CAML_FLAGS= -w A -warn-error $(WARN_ERROR) -dtypes
CAML_INCLUDES=-I $(STDLIB_DIR) -I $(ALGEBRALIB_DIR) -I $(UTILSLIB_DIR)

ZENON_LIB_DIR= $(shell zenon -where)
COQ_INCLUDES=\
 -I $(ZENON_LIB_DIR) -I $(STDLIB_DIR) -I $(ALGEBRALIB_DIR)\
 -I $(UTILSLIB_DIR)

include $(ROOT_DIR)/Makefile.common
include $(ROOT_DIR)/focalizec/.install_var
include $(ROOT_DIR)/focalizec/.config_var

FOCALIZE_SRC=\
 etat_vote.fcl\
 num_capteur.fcl\
 basic_type.fcl\
 gen_value.fcl\
 valid_meas.fcl\
 value.fcl\
 value_with_valid.fcl\
 gen_diag.fcl\
 diag.fcl\
 gen_vote.fcl\
 vote.fcl\
 main.fcl

FOCALIZE_FOS=$(FOCALIZE_SRC:.fcl=.fo)

FOCALIZE_MLS=$(FOCALIZE_SRC:.fcl=.ml)

FOCALIZE_BYT_OBJS=$(FOCALIZE_MLS:.ml=.cmo)

FOCALIZE_BIN_OBJS=$(FOCALIZE_BYT_OBJS:.cmo=.cmx)

FOCALIZE_ZVS=$(FOCALIZE_SRC:.fcl=.zv)

FOCALIZE_VS=$(FOCALIZE_ZVS:.zv=.v)

FOCALIZE_VOS=$(FOCALIZE_VS:.v=.vo)

FOCALIZE_ZSKS=$(FOCALIZE_SRC:.fcl=.sk.zv)

FOCALIZE_SKS=$(FOCALIZE_SRC:.fcl=.sk)

FOCALIZE_DKOS=$(FOCALIZE_SKS:.sk=.dko)

EXTERNAL_BYT_OBJS=\
 ../utils/pair.cmo\
 ../utils/peano.cmo

EXTERNAL_BIN_OBJS=$(EXTERNAL_BYT_OBJS:.cmo=.cmx)

EXTERNAL_VOS=\
 ../utils/pair.vo\
 ../utils/peano.vo

STD_BIN_OBJS=\
 ml_builtins.cmx basics.cmx\
 constants.cmx orders.cmx\
 lattices.cmx\
 sets_externals.cmx sets.cmx\
 multiplicative_law.cmx additive_law.cmx\
 integers.cmx iterators.cmx\
 weak_structures_externals.cmx weak_structures.cmx\
 rings_fields.cmx

BIN_EXE=build.bin


all: $(BIN_EXE)

lib: all

byt:\
 $(EXTERNAL_BYT_OBJS) $(EXTERNAL_VOS)\
 $(FOCALIZE_MLS) $(FOCALIZE_ZVS)\
 $(FOCALIZE_BYT_OBJS) $(FOCALIZE_VS) $(FOCALIZE_VOS)\
 $(FOCALIZE_SKS) $(FOCALIZE_DKOS)

bin:\
 $(EXTERNAL_BIN_OBJS) $(EXTERNAL_VOS)\
 $(FOCALIZE_MLS) $(FOCALIZE_ZVS)\
 $(FOCALIZE_BIN_OBJS) $(FOCALIZE_VS) $(FOCALIZE_VOS)\
 $(FOCALIZE_SKS) $(FOCALIZE_DKOS)

$(BIN_EXE): byt bin
	$(CAMLBIN) -o $@\
	   -I $(STDLIB_DIR) -I $(ALGEBRALIB_DIR) -I $(UTILSLIB_DIR)\
	    $(STD_BIN_OBJS) $(EXTERNAL_BIN_OBJS) $(FOCALIZE_BIN_OBJS)

install:
	@$(MKDIR) $(INSTALL_LIB_DIR) &&\
	if [ "$(FOCALIZE_FOS)" != "" ] ; then\
	  echo "Installing (FoCaLize) compiled certified libs." &&\
	  $(CP) $(FOCALIZE_FOS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_FOS_UNSAFE)" != "" ] ; then\
	  echo "Installing (FoCaLize) compiled non-certified libs." &&\
	  $(CP) $(FOCALIZE_FOS_UNSAFE) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_BYT_OBJS)" != "" ] ; then\
	  $(CP) $(FOCALIZE_BYT_OBJS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_BYT_OBJS_UNSAFE)" != "" ] ; then\
	  $(CP) $(FOCALIZE_BYT_OBJS_UNSAFE) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_BIN_OBJS)" != "" ] ; then\
	  $(CP) $(FOCALIZE_BIN_OBJS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_BIN_OBJS_UNSAFE)" != "" ] ; then\
	  $(CP) $(FOCALIZE_BIN_OBJS_UNSAFE) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_VOS)" != "" ] ; then\
	  $(CP) $(FOCALIZE_VOS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(EXTERNAL_BYT_OBJS)" != "" ] ; then\
	  $(CP) $(EXTERNAL_BYT_OBJS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(EXTERNAL_BIN_OBJS)" != "" ] ; then\
	  $(CP) $(EXTERNAL_BIN_OBJS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(EXTERNAL_VOS)" != "" ] ; then\
	  $(CP) $(EXTERNAL_VOS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(EXTERNAL_DKOS)" != "" ] ; then\
	  $(CP) $(EXTERNAL_DKOS) $(INSTALL_LIB_DIR)/;\
	fi

clean:
	$(RM) $(FOCALIZE_MLS) $(FOCALIZE_ZVS) $(FOCALIZE_VS) \
	$(FOCALIZE_ZDKS) $(FOCALIZE_DKS)
	$(RM) *.cm* *.o *.a *.annot *.out *.output *.bin *.byt *.zv *.vo
	$(RM) *.glob *.fcd *.pfc *.fo .depend

depend: .depend

.depend:
	$(CAMLDEP) *.mli *.ml > ./.depend
	$(FOCALIZEDEP) *.fcl >> ./.depend

-include .depend
