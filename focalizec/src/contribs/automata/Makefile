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

# $Id: Makefile,v 1.9 2012-10-15 14:58:37 pessaux Exp $

# Makefile for algebra structures implementations

ROOT_DIR=../../../..
SRC_DIR=$(ROOT_DIR)/focalizec/src

include $(ROOT_DIR)/Makefile.common
INSTALL_LIB_DIR=$(LIB_DIR)/automata

STDLIB_DIR=$(SRC_DIR)/stdlib

ALGEBRALIB_DIR=$(SRC_DIR)/extlib/algebra

UTILSLIB_DIR=../utils

FOCALIZE_COMPILER=$(SRC_DIR)/toplevel/focalizec

FOCALIZE_COMP_FLAGS=-no-stdlib-path -I $(STDLIB_DIR)

FOCALIZEC=$(FOCALIZE_COMPILER) $(FOCALIZE_COMP_FLAGS)
FOCALIZEDEP=../../focalizedep/focalizedep

#CAML_FLAGS=-w A -warn-error $(WARN_ERROR) -dtypes -I $(STDLIB_DIR)
CAML_FLAGS=-dtypes -I $(STDLIB_DIR)

# From Makefile.config ZENON_LIB_DIR
# Was ZENON_LIB_DIR=$(ROOT_DIR)/../zenon/
ZENON_LIB_DIR=$(shell zenon -where)
COQ_INCLUDES=-I $(ZENON_LIB_DIR) -I $(STDLIB_DIR)

FOCALIZE_SRC=\
 gen_def.fcl\
 request.fcl\
 switch_recovery_normal_automata.fcl\
 switch_recovery_reverse_automata.fcl\
 switch_recovery_automata.fcl\
 #switch_automata.fcl ## Still buggy \
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

EXTERNAL_BYT_OBJS=

EXTERNAL_BIN_OBJS=$(EXTERNAL_BYT_OBJS:.cmo=.cmx)

EXTERNAL_VOS=\
 ../utils/pair.vo\
 ../utils/peano.vo

EXTERNAL_DKOS=\
 ../utils/pair.dko\
 ../utils/peano.dko

STD_BIN_OBJS=\
 ml_builtins.cmx basics.cmx\
 constants.cmx orders.cmx\
 lattices.cmx\
 sets_externals.cmx sets.cmx\
 sets_orders_externals.cmx sets_orders.cmx\
 multiplicative_law.cmx additive_law.cmx\
 integers.cmx iterators.cmx\
 weak_structures_externals.cmx weak_structures.cmx\
 rings_fields.cmx

BINARY=build.x

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
	if [ "$(FOCALIZE_CMIS)" != "" ] ; then\
	  echo "Installing (Ocaml interface) compiled certified libs." &&\
	  $(CP) $(FOCALIZE_CMIS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_CMIS_UNSAFE)" != "" ] ; then\
	  echo "Installing (Ocaml interface) compiled non-certified libs." &&\
	  $(CP) $(FOCALIZE_CMIS_UNSAFE) $(INSTALL_LIB_DIR)/;\
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
	if [ "$(EXTERNAL_CMIS)" != "" ] ; then\
	  $(CP) $(EXTERNAL_CMIS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(EXTERNAL_BYT_OBJS)" != "" ] ; then\
	  $(CP) $(EXTERNAL_BYT_OBJS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(EXTERNAL_BIN_OBJS)" != "" ] ; then\
	  $(CP) $(EXTERNAL_BIN_OBJS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(EXTERNAL_VOS)" != "" ] ; then\
	  $(CP) $(EXTERNAL_VOS) $(INSTALL_LIB_DIR)/;\
	fi

clean:
	$(RM) $(FOCALIZE_MLS) $(FOCALIZE_ZVS) $(FOCALIZE_VS)
	$(RM) $(FOCALIZE_BYT_OBJS) $(FOCALIZE_BIN_OBJS) $(FOCALIZE_VOS) $(FOCALIZE_DKOS)
	$(RM) $(EXTERNAL_BYT_OBJS) $(EXTERNAL_BIN_OBJS) $(EXTERNAL_VOS) $(EXTERNAL_DKOS)
	$(RM) big_integers.ml parse_poly.ml polys_abstract.ml
	$(RM) big_integers.cm* parse_poly.cm* polys_abstract.cm*
	$(RM) big_integers.o parse_poly.o polys_abstract.o
	$(RM) *.annot *.glob *.zv *.pfc .depend
	$(RM) *.cm* *.o *.a *.annot *.out *.output *.bin *.byt


# Were not compiled to Coq in the previous FoCaL compiler...
big_integers.ml: big_integers.fcl
	$(FOCALIZEC) --no-coq-code --no-dedukti-code $<
parse_poly.ml: parse_poly.fcl
	$(FOCALIZEC) --no-coq-code --no-dedukti-code $<
polys_abstract.ml: polys_abstract.fcl
	$(FOCALIZEC) --no-coq-code --no-dedukti-code $<
#End temporary

# Should be generated ?
randoms_externals.vo: $(STDLIB_DIR)/basics.vo
arrays_externals.vo: $(STDLIB_DIR)/basics.vo
randoms_externals.dko: $(STDLIB_DIR)/basics.dko
arrays_externals.dko: $(STDLIB_DIR)/basics.dko
# End Should be generated ?

build.x: bin
	$(CAML_BIN_COMPILER) -o $@\
           -I $(STDLIB_DIR) -I $(ALGEBRALIB_DIR) -I $(UTILSLIB_DIR)\
            $(STD_BIN_OBJS) $(EXTERNAL_BIN_OBJS) $(FOCALIZE_BIN_OBJS)

depend: .depend

.depend:
	$(CAMLDEP) *.mli *.ml > ./.depend
	$(FOCALIZEDEP) *.fcl >> ./.depend

-include .depend
