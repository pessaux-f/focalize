#**********************************************************************#
#                                                                      #
#                        FoCaLize compiler                             #
#                                                                      #
#            Fran�ois Pessaux                                          #
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

LIB_INSTALL_SUB_DIR=algebra

ROOT_DIR=../../../..

PROJECT_LIB_INSTALL_DIR=$(LIB_DIR)/$(PROJECT_NAME_FULL)

LIB_INSTALL_DIR=$(PROJECT_LIB_INSTALL_DIR)/$(LIB_INSTALL_SUB_DIR)

STDLIB_DIR=../../stdlib

FOCALIZE_SRC=\
 constants.fcl\
 additive_law.fcl\
 multiplicative_law.fcl\
 weak_structures.fcl \
 rings_fields.fcl\
 randoms.fcl\
 integers.fcl\
 arrays.fcl\
 product_structures.fcl\
 quotient_structures.fcl\
 small_integers.fcl\
 iterators.fcl

# FOCALIZE_SRC_UNSAFE are compiled not using the implicit rules. They have
# their own rules at the end of this Makefile.
FOCALIZE_SRC_UNSAFE= \
big_integers.fcl

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

EXTERNAL_BYT_OBJS=\
 weak_structures_externals.cmo\
 randoms_externals.cmo
EXTERNAL_CMIS=$(EXTERNAL_BYT_OBJS:.cmo=.cmi)

EXTERNAL_BIN_OBJS=$(EXTERNAL_BYT_OBJS:.cmo=.cmx)

EXTERNAL_VOS=\
 weak_structures_externals.vo\
 randoms_externals.vo arrays_externals.vo

include $(ROOT_DIR)/Makefile.common

FOCALIZEC=../../toplevel/focalizec
FOCALIZE_COMP_FLAGS= -no-stdlib-path -I $(STDLIB_DIR) \
	-coq-version $(COQ_VERSION) -focalize-doc

CAMLBIN=ocamlopt
CAMLBYT=ocamlc
CAML_FLAGS=-w A -warn-error  $(WARN_ERROR) -dtypes -I $(STDLIB_DIR)
CAML_INCLUDES=-I $(STDLIB_DIR) -I `ocamlfind query num`

ZVTOV_COMP_FLAGS=-script -zenon $(ZENON_COMPILER) -new
ZENON_LIB_DIR=$(shell $(ZENON) -where)

# Bug hack to compare version string. Indeed, lexicographic order sucks since
# for example 8.12 < 8.5.
VERSION_LT=bash -c '\
  if [ "$$1" = "$$2" ] ; \
  then return 1 \
  fi \
  SORTED="`echo "$$1\n$$2" | sort -V | head -n1`" \
  if [ "$$1" = "$SORTED" ] ; \
  then \
      return 0 \
  fi \
  return 1' VERSION_LT

ifeq ($(shell test "$(COQ_VERSION)" != "" -a ${VERSION_LT} "$(COQ_VERSION)" "8.5"; echo $$?),0)
	COQ_INCLUDES=-I $(ZENON_LIB_DIR) -I $(STDLIB_DIR)
else
	COQ_INCLUDES=-Q $(ZENON_LIB_DIR) "" -Q $(STDLIB_DIR) ""
endif

SKDEP_CALL=true
ifneq ($(SKCHECK),)
ifneq ($(DKCHECK),)
ifneq ($(ZENONM),)
FOCALIZE_ZSKS=$(FOCALIZE_SRC:.fcl=.sk.zv)
FOCALIZE_ZSKS_UNSAFE=$(FOCALIZE_SRC_UNSAFE:.fcl=.sk.zv)

FOCALIZE_SKS=$(FOCALIZE_SRC:.fcl=.sk)
FOCALIZE_SKS_UNSAFE=$(FOCALIZE_SRC_UNSAFE:.fcl=.sk)

FOCALIZE_DKOS=$(FOCALIZE_SKS:.sk=.dko)
FOCALIZE_DKOS_UNSAFE=$(FOCALIZE_SRC_UNSAFE:.fcl=.dko)

EXTERNAL_DKOS=\
 weak_structures_externals.dko\
 randoms_externals.dko arrays_externals.dko

SKDEP_CALL= $(SKDEP) -I $(STDLIB_DIR) --ignore $(EXTERNAL_DKOS:.dko=.sk) >> .depend

FOCALIZE_COMP_FLAGS += -dedukti-code

DK_INCLUDES=-I $(shell $(ZENONM) -where) -I $(STDLIB_DIR)
endif
endif
endif

lib: all

focalizedoc:\
 $(FOCALIZE_MML) $(FOCALIZE_MML_UNSAFE)

byt:\
 $(EXTERNAL_BYT_OBJS) $(EXTERNAL_VOS)\
 $(EXTERNAL_DKOS)\
 $(FOCALIZE_FOS)\
 $(FOCALIZE_MLS) $(FOCALIZE_MLS_UNSAFE)\
 $(FOCALIZE_ZVS)\
 $(FOCALIZE_BYT_OBJS) $(FOCALIZE_BYT_OBJS_UNSAFE)\
 $(FOCALIZE_VS) $(FOCALIZE_VOS)\
 $(FOCALIZE_SKS) $(FOCALIZE_DKOS)

bin:\
 $(EXTERNAL_BIN_OBJS) $(EXTERNAL_VOS)\
 $(EXTERNAL_DKOS)\
 $(FOCALIZE_FOS)\
 $(FOCALIZE_MLS) $(FOCALIZE_MLS_UNSAFE)\
 $(FOCALIZE_ZVS)\
 $(FOCALIZE_BIN_OBJS) $(FOCALIZE_BIN_OBJS_UNSAFE)\
 $(FOCALIZE_VS) $(FOCALIZE_VOS)\
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
	$(RM) $(FOCALIZE_MLS) $(FOCALIZE_ZVS) $(FOCALIZE_VS) $(FOCALIZE_ZSKS) $(FOCALIZE_SKS)
	$(RM) $(FOCALIZE_MLS_UNSAFE) $(FOCALIZE_ZVS_UNSAFE) $(FOCALIZE_VS_UNSAFE) $(FOCALIZE_ZSKS_UNSAFE) $(FOCALIZE_SKS_UNSAFE)
	$(RM) *.cm* *.o *.a *.annot *.out *.output *.bin *.byt *.vo *.zv *.dko *.glob *.pfc *~ .*~ *.fo .depend

# Do not yet compile in Coq
big_integers.ml: big_integers.fcl
	$(FOCALIZEC) -no-stdlib-path $(CAML_INCLUDES) -no-coq-code -no-dedukti-code $<
#End temporary

# Should be generated ?
randoms_externals.vo: $(STDLIB_DIR)/basics.vo
arrays_externals.vo: $(STDLIB_DIR)/basics.vo
# End Should be generated ?

depend:
	../../focalizedep/focalizedep $(FOCALIZE_SRC) iterators.fcl > .depend
	$(SKDEP_CALL)

.depend: depend

-include .depend
