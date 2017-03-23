# ************************************************************************** #
#                                                                            #
#                        FoCaLiZe compiler                                   #
#                                                                            #
#            François Pessaux                                                #
#            Pierre Weis                                                     #
#            Damien Doligez                                                  #
#                                                                            #
#                 LIP6  --  INRIA Rocquencourt - ENSTA ParisTech             #
#                                                                            #
#  Copyright 2007 - ... LIP6 and INRIA                                       #
#            2012 - ... ENSTA ParisTech                                      #
#  Distributed only by permission.                                           #
#                                                                            #
# ************************************************************************** #

# Makefile for the FoCaLize standard library

ROOT_DIR=../../..

FOCALIZE_SRC=\
 basics.fcl \
 wellfoundation.fcl \
 sets.fcl \
 products.fcl \
 lattices.fcl \
 orders.fcl \
 strict_orders.fcl \
 orders_and_lattices.fcl \
 sums.fcl \
 quotients.fcl \
 fix.fcl

FOCALIZE_FOS=$(FOCALIZE_SRC:.fcl=.fo)
FOCALIZE_MLS=$(FOCALIZE_SRC:.fcl=.ml)

FOCALIZE_CMIS=$(FOCALIZE_SRC:.fcl=.cmi)

FOCALIZE_BYT_OBJS=$(FOCALIZE_MLS:.ml=.cmo)
FOCALIZE_BIN_OBJS=$(FOCALIZE_BYT_OBJS:.cmo=.cmx)

FOCALIZE_ZVS=$(FOCALIZE_SRC:.fcl=.zv)
FOCALIZE_VS=$(FOCALIZE_ZVS:.zv=.v)
FOCALIZE_VOS=$(FOCALIZE_VS:.v=.vo)

FOCALIZE_DOCS=$(FOCALIZE_SRC:.fcl=.fcd)

FOCALIZE_MML=$(FOCALIZE_DOCS:.fcd=.xml)

EXTERNAL_BYT_OBJS=\
 ml_builtins.cmo sets_externals.cmo

EXTERNAL_CMIS=$(EXTERNAL_BYT_OBJS:.cmo=.cmi)
EXTERNAL_BIN_OBJS=$(EXTERNAL_BYT_OBJS:.cmo=.cmx)
EXTERNAL_CMIS=$(EXTERNAL_BYT_OBJS:.cmo=.cmi)

EXTERNAL_VOS=\
 coq_builtins.vo\
 sets_externals.vo

include $(ROOT_DIR)/Makefile.common

FOCALIZEC=../toplevel/focalizec
FOCALIZE_COMP_FLAGS=-no-stdlib-path \
 -stop-before-coq -stop-before-dk \
 -coq-version $(COQ_VERSION) -focalize-doc
FOCALIZEDEP=../focalizedep/focalizedep

ifeq ($(shell test "$(COQ_VERSION)" != "" -a "$(COQ_VERSION)" \< "8.5"; echo $$?),0)
	COQ_INCLUDES= -I $(shell $(ZENON) -where)
else
	COQ_INCLUDES= -Q $(shell $(ZENON) -where) ""
endif

SKDEP_CALL=true
ifneq ($(SKCHECK),)
ifneq ($(DKCHECK),)
ifneq ($(ZENONM),)
FOCALIZE_SKZVS=$(FOCALIZE_SRC:.fcl=.sk.zv)
FOCALIZE_DKS=$(FOCALIZE_SRC:.fcl=.sk)
FOCALIZE_DKOS=$(FOCALIZE_DKS:.sk=.dko)
EXTERNAL_DKOS=\
 cc.dko\
 builtins.dko\
 dk_bool.dko\
 dk_fail.dko\
 dk_opt.dko\
 dk_tuple.dko\
 dk_sum.dko\
 dk_logic.dko\
 dk_list.dko\
 dk_nat.dko\
 dk_int.dko\
 dk_binary_nat.dko\
 dk_machine_int.dko\
 dk_char.dko\
 dk_string.dko\
 dk_builtins.dko\
 zen.dko\
 focal.dko\
 sets_externals.dko\
 wellfoundation_externals.dko

cc.dko: cc.dk
	$(DKCHECK) -e $<

builtins.dko: builtins.dk cc.dko
	$(DKCHECK) -e $<

SKDEP_CALL=$(SKDEP) -v *.sk >> ./.depend

FOCALIZE_COMP_FLAGS += -dedukti-code
endif
endif
endif


lib: all

# Disable DTD validation because it's too long.
XSLTPROC_FLAGS=-novalid
FD2HTML=$(ROOT_DIR)/focalizec/src/docgen/focdoc2html.xsl
MMLC2P=$(ROOT_DIR)/focalizec/src/docgen/mmlctop2_0.xsl

focalizedoc:\
 $(FOCALIZE_MML)

byt:\
 $(EXTERNAL_BYT_OBJS) $(EXTERNAL_VOS) $(EXTERNAL_DKOS)\
 $(FOCALIZE_MLS)\
 $(FOCALIZE_BYT_OBJS)\
 $(FOCALIZE_VS) $(FOCALIZE_VOS)\
 $(FOCALIZE_DKS) $(FOCALIZE_DKOS)

bin:\
 $(EXTERNAL_BIN_OBJS) $(EXTERNAL_VOS) $(EXTERNAL_DKOS)\
 $(FOCALIZE_MLS)\
 $(FOCALIZE_BIN_OBJS)\
 $(FOCALIZE_VS) $(FOCALIZE_VOS)\
 $(FOCALIZE_DKS) $(FOCALIZE_DKOS)

install:
	@$(MKDIR) $(INSTALL_LIB_DIR) &&\
	if [ "$(FOCALIZE_FOS)" != "" ]; then\
	  echo "Installing (FoCaLize) compiled certified libs." &&\
	  $(CP) $(FOCALIZE_FOS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_CMIS)" != "" ]; then\
	  echo "Installing (OCaml interface) compiled certified libs." &&\
	  $(CP) $(FOCALIZE_CMIS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_BYT_OBJS)" != "" ]; then\
	  echo "Installing (OCaml bytecode) compiled certified libs." &&\
	  $(CP) $(FOCALIZE_BYT_OBJS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_BIN_OBJS)" != "" ]; then\
	  echo "Installing (OCaml native) compiled certified libs." &&\
	  $(CP) $(FOCALIZE_BIN_OBJS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_DKOS)" != "" ]; then\
	  echo "Installing (Dedukti) compiled certified libs." &&\
	  $(CP) $(FOCALIZE_DKOS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(FOCALIZE_VOS)" != "" ]; then\
	  echo "Installing (Coq) compiled certified libs." &&\
	  $(CP) $(FOCALIZE_VOS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(EXTERNAL_CMIS)" != "" ]; then\
	  echo "Installing (OCaml interface) external stubs." &&\
	  $(CP) $(EXTERNAL_CMIS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(EXTERNAL_BYT_OBJS)" != "" ]; then\
	  echo "Installing (OCaml bytecode) external stubs." &&\
	  $(CP) $(EXTERNAL_BYT_OBJS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(EXTERNAL_BIN_OBJS)" != "" ]; then\
	  echo "Installing (OCaml native) external stubs." &&\
	  $(CP) $(EXTERNAL_BIN_OBJS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(EXTERNAL_VOS)" != "" ]; then\
	  echo "Installing (Coq) external stubs." &&\
	  $(CP) $(EXTERNAL_VOS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	if [ "$(EXTERNAL_DKOS)" != "" ]; then\
	  echo "Installing (Dedukti) external stubs." &&\
	  $(CP) $(EXTERNAL_DKOS) $(INSTALL_LIB_DIR)/;\
	fi

# TODO
uninstall:
	echo "Uninstall stdlib TODO"

clean:
	$(RM) *zvtmp* *.cm* *.o *.a *.annot *.out *.output *.bin *.byt *.fo *.vo *.dko *.pfc *.glob *~ .*~
	$(RM) $(FOCALIZE_MLS) $(FOCALIZE_ZVS) $(FOCALIZE_SKZVS) $(FOCALIZE_VS) $(FOCALIZE_DKS)

depend:
	$(CAMLDEP) *.mli *.ml > ./.depend
	$(FOCALIZEDEP) *.fcl >> ./.depend
	$(SKDEP_CALL)

include .depend
