# ************************************************************************** #
#                                                                            #
#                        FoCaLiZe compiler                                   #
#                                                                            #
#            François Pessaux                                                #
#            Pierre Weis                                                     #
#            Damien Doligez                                                  #
#            Renaud Rioobo                                                   #
#                                                                            #
#                 LIP6  --  INRIA Rocquencourt - ENSTA ParisTech             #
#                                                                            #
#  Copyright 2007 - ... LIP6 and INRIA                                       #
#            2012 - ... ENSTA ParisTech                                      #
#  Distributed only by permission.                                           #
#                                                                            #
# ************************************************************************** #


# Makefile for algebra structures implementations

LIB_INSTALL_SUB_DIR=polynomials

ROOT_DIR=../../../..

PROJECT_LIB_INSTALL_DIR=$(LIB_DIR)/$(PROJECT_NAME_FULL)

LIB_INSTALL_DIR=$(PROJECT_LIB_INSTALL_DIR)/$(LIB_INSTALL_SUB_DIR)

STDLIB_DIR=$(ROOT_DIR)/focalizec/src/stdlib

FOCALIZE_COMPILER=$(ROOT_DIR)/focalizec/src/toplevel/focalizec

FOCALIZE_COMP_FLAGS=\
  -no-stdlib-path \
  -I $(STDLIB_DIR) $(FOCALIZEC_COQ_COMPATIBILITY_OPTION) \
  -I $(ROOT_DIR)/focalizec/src/extlib/algebra

FOCALIZEC=$(FOCALIZE_COMPILER) $(FOCALIZE_COMP_FLAGS)
FOCALIZEDEP=../../focalizedep/focalizedep

CAML_FLAGS= -w A -warn-error $(WARN_ERROR) -dtypes
CAML_INCLUDES=-I $(STDLIB_DIR) -I $(ROOT_DIR)/focalizec/src/extlib/algebra

ZENON_LIB_DIR= $(shell zenon -where)
ZENON_MODULO_LIB_DIR= $(shell zenon_modulo -where)
ZVTOV=zvtov
ZVTOV_COMP_FLAGS=-zenon $(ZENON_COMPILER) -new

COQ_INCLUDES=\
 -I $(ZENON_LIB_DIR) -I $(STDLIB_DIR) \
 -I $(ROOT_DIR)/focalizec/src/extlib/algebra

DK_INCLUDES=\
 -I $(ZENON_MODULO_LIB_DIR) -I $(STDLIB_DIR) \
 -I $(ROOT_DIR)/focalizec/src/extlib/algebra

include $(ROOT_DIR)/Makefile.common
include $(ROOT_DIR)/focalizec/.install_var
include $(ROOT_DIR)/.config_var

FOCALIZE_SRC=\
 parse_poly.fcl\
 polys_abstract.fcl

# FOCALIZE_SRC_UNSAFE are compiled not using the implicit rules. They have
# their own rules at the end of this Makefile.
FOCALIZE_SRC_UNSAFE=\
 distributed_polys.fcl\
 univariate_polys.fcl\
 recursive_polys.fcl

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

FOCALIZE_ZSKS=$(FOCALIZE_SRC:.fcl=.sk.zv)
FOCALIZE_ZSKS_UNSAFE=$(FOCALIZE_SRC_UNSAFE:.fcl=.sk.zv)

FOCALIZE_SKS=$(FOCALIZE_SRC:.fcl=.sk)
FOCALIZE_SKS_UNSAFE=$(FOCALIZE_SRC_UNSAFE:.fcl=.sk)

FOCALIZE_DKOS=$(FOCALIZE_SKS:.dk=.dko)
FOCALIZE_DKOS_UNSAFE=$(FOCALIZE_SKS_UNSAFE:.sk=.dko)

FOCALIZE_DOCS=$(FOCALIZE_SRC:.fcl=.fcd)
FOCALIZE_DOCS_UNSAFE=$(FOCALIZE_SRC_UNSAFE:.fcl=.fcd)

FOCALIZE_MML=$(FOCALIZE_DOCS:.fcd=.xml)
FOCALIZE_MML_UNSAFE=$(FOCALIZE_DOCS_UNSAFE:.fcd=.xml)

EXTERNAL_BYT_OBJS=
EXTERNAL_CMIS=$(EXTERNAL_BYT_OBJS:.cmo=.cmi)

EXTERNAL_BIN_OBJS=$(EXTERNAL_BYT_OBJS:.cmo=.cmx)

EXTERNAL_VOS=


lib: all

focalizedoc: \
	$(FOCALIZE_MML) $(FOCALIZE_MML_UNSAFE)

byt: \
      $(EXTERNAL_BYT_OBJS) $(EXTERNAL_VOS)\
      $(FOCALIZE_MLS) $(FOCALIZE_MLS_UNSAFE)\
      $(FOCALIZE_ZVS)\
      $(FOCALIZE_BYT_OBJS) $(FOCALIZE_BYT_OBJS_UNSAFE)\
      $(FOCALIZE_VS) $(FOCALIZE_VOS)\
      $(FOCALIZE_SKS) $(FOCALIZE_DKOS)

bin: \
      $(EXTERNAL_BIN_OBJS) $(EXTERNAL_VOS)\
      $(FOCALIZE_MLS) $(FOCALIZE_MLS_UNSAFE)\
      $(FOCALIZE_ZVS)\
      $(FOCALIZE_BIN_OBJS) $(FOCALIZE_BIN_OBJS_UNSAFE)\
      $(FOCALIZE_VS) $(FOCALIZE_VOS)\
      $(FOCALIZE_SKS) $(FOCALIZE_DKOS)

install:
	@$(MKDIR) $(INSTALL_LIB_DIR) &&\
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
	  $(CP) $(FOCALIZE_CMIS) $(INSTALL_LIB_DIR)/; \
	fi && \
	if [ "$(FOCALIZE_CMIS_UNSAFE)" != "" ] ; then \
	  echo "Installing (Ocaml interface) compiled non-certified libs." && \
	  $(CP) $(FOCALIZE_CMIS_UNSAFE) $(INSTALL_LIB_DIR)/; \
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
	fi && \
	if [ "$(FOCALIZE_DKOS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_DKOS) $(INSTALL_LIB_DIR)/; \
	fi && \
	if [ "$(EXTERNAL_CMIS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_CMIS) $(INSTALL_LIB_DIR)/; \
	fi && \
	if [ "$(EXTERNAL_BYT_OBJS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_BYT_OBJS) $(INSTALL_LIB_DIR)/; \
	fi && \
	if [ "$(EXTERNAL_BIN_OBJS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_BIN_OBJS) $(INSTALL_LIB_DIR)/; \
	fi && \
	if [ "$(EXTERNAL_VOS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_VOS) $(INSTALL_LIB_DIR)/; \
	fi && \
	if [ "$(EXTERNAL_DKOS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_DKOS) $(INSTALL_LIB_DIR)/; \
	fi

clean:
	$(RM) $(FOCALIZE_MLS) $(FOCALIZE_ZVS) $(FOCALIZE_VS)
	$(RM) $(FOCALIZE_BYT_OBJS) $(FOCALIZE_BIN_OBJS) $(FOCALIZE_VOS) $(FOCALIZE_DKOS)
	$(RM) $(FOCALIZE_MLS_UNSAFE) $(FOCALIZE_ZVS_UNSAFE) $(FOCALIZE_VS_UNSAFE) $(FOCALIZE_ZSKS_UNSAFE) $(FOCALIZE_SKS_UNSAFE)
	$(RM) $(FOCALIZE_BYT_OBJS_UNSAFE) $(FOCALIZE_BIN_OBJS_UNSAFE) $(FOCALIZE_VOS_UNSAFE) $(FOCALIZE_DKOS_UNSAFE)
	$(RM) $(EXTERNAL_BYT_OBJS) $(EXTERNAL_BIN_OBJS) $(EXTERNAL_VOS) $(EXTERNAL_DKOS)
	$(RM) $(EXTERNAL_CMIS) $(FOCALIZE_CMIS_UNSAFE) $(FOCALIZE_CMIS)
	$(RM) big_integers.ml parse_poly.ml polys_abstract.ml
	$(RM) big_integers.cm* parse_poly.cm* polys_abstract.cm*
	$(RM) big_integers.o parse_poly.o polys_abstract.o
	$(RM) *.annot *.glob *.fcd *.zv *.vo *.fo *.pfc *.dko
	$(RM) *.cm* *.o *.a *.annot *.out *.output *.bin *.byt .depend

recursive_polys.ml: recursive_polys.fcl
	$(FOCALIZEC) -no-coq-code -no-dedukti-code $<

recursive_polys.fcd: recursive_polys.fcl
	$(FOCALIZEC) -focalize-doc -no-coq-code -no-dedukti-code $<

univariate_polys.ml: univariate_polys.fcl
	$(FOCALIZEC) -no-coq-code -no-dedukti-code $<

univariate_polys.fcd: univariate_polys.fcl
	$(FOCALIZEC) -focalize-doc -no-coq-code -no-dedukti-code $<

distributed_polys.ml: distributed_polys.fcl
	$(FOCALIZEC) -no-coq-code -no-dedukti-code $<

distributed_polys.fcd: distributed_polys.fcl
	$(FOCALIZEC) -focalize-doc -no-coq-code -no-dedukti-code $<

depend: .depend

.depend:
	$(CAMLDEP) *.mli *.ml > ./.depend
	$(FOCALIZEDEP) *.fcl >> ./.depend

-include .depend
