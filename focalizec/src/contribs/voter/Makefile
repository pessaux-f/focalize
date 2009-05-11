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

# $Id: Makefile,v 1.8 2009-05-11 08:54:20 pessaux Exp $

# Makefile for algebra structures implementations

ROOT_DIR = ../../..

include $(ROOT_DIR)/Makefile.config

LIB_INSTALL_DIR = $(LIB_DIR)/voter

STDLIB_DIR = $(ROOT_DIR)/src/stdlib

ALGEBRALIB_DIR = $(ROOT_DIR)/src/extlib/algebra

UTILSLIB_DIR = ../utils

FOCALIZE_COMPILER = $(ROOT_DIR)/src/toplevel/focalizec

FOCALIZE_COMP_FLAGS =\
  -no-stdlib-path -I $(STDLIB_DIR) -I $(ALGEBRALIB_DIR)\
  -I $(UTILSLIB_DIR)

# From Makefile.config FOCALIZEC = $(FOCALIZE_COMPILER) $(FOCALIZE_COMP_FLAGS)

CAML_COMP_FLAGS =\
  -w A -warn-error Ae -dtypes -I $(STDLIB_DIR)\
  -I $(ALGEBRALIB_DIR) -I $(UTILSLIB_DIR)

# From Makefile.config ZENON_LIB_DIR
# Was ZENON_LIB_DIR = $(ROOT_DIR)/../zenon/

COQ_INCLUDES =\
  -I $(ZENON_LIB_DIR) -I $(STDLIB_DIR) -I $(ALGEBRALIB_DIR)\
  -I $(UTILSLIB_DIR)

FOCALIZE_SRC =\
  etat_vote.fcl\
  num_capteur.fcl\
  value.fcl\
  vote.fcl\
  main.fcl

FOCALIZE_FOS = $(FOCALIZE_SRC:.fcl=.fo)

FOCALIZE_MLS = $(FOCALIZE_SRC:.fcl=.ml)

FOCALIZE_BYT_OBJS = $(FOCALIZE_MLS:.ml=.cmo)

FOCALIZE_BIN_OBJS = $(FOCALIZE_BYT_OBJS:.cmo=.cmx)

FOCALIZE_ZVS = $(FOCALIZE_SRC:.fcl=.zv)

FOCALIZE_VS = $(FOCALIZE_ZVS:.zv=.v)

FOCALIZE_VOS = $(FOCALIZE_VS:.v=.vo)

EXTERNAL_BYT_OBJS =\
  ../utils/pair.cmo\
  ../utils/peano.cmo

EXTERNAL_BIN_OBJS = $(EXTERNAL_BYT_OBJS:.cmo=.cmx)

EXTERNAL_VOS =\
  ../utils/pair.vo\
  ../utils/peano.vo

STD_BIN_OBJS =\
  ml_builtins.cmx basics.cmx\
  constants.cmx orders.cmx\
  lattices.cmx\
  sets_externals.cmx sets.cmx\
  sets_orders_externals.cmx sets_orders.cmx\
  multiplicative_law.cmx additive_law.cmx\
  integers.cmx iterators.cmx\
  weak_structures_externals.cmx weak_structures.cmx\
  rings_fields.cmx

BIN_EXE = build.x

include $(ROOT_DIR)/Makefile.common

all:: $(BIN_EXE)

lib: all

byt:: \
  $(EXTERNAL_BYT_OBJS) $(EXTERNAL_VOS)\
  $(FOCALIZE_MLS) $(FOCALIZE_ZVS)\
  $(FOCALIZE_BYT_OBJS) $(FOCALIZE_VS) $(FOCALIZE_VOS)

bin:: \
  $(EXTERNAL_BIN_OBJS) $(EXTERNAL_VOS)\
  $(FOCALIZE_MLS) $(FOCALIZE_ZVS)\
  $(FOCALIZE_BIN_OBJS) $(FOCALIZE_VS) $(FOCALIZE_VOS)

$(BIN_EXE): byt
	$(CAML_BIN_COMPILER) -o $@ \
           -I $(STDLIB_DIR) -I $(ALGEBRALIB_DIR) -I $(UTILSLIB_DIR)\
            $(STD_BIN_OBJS) $(EXTERNAL_BIN_OBJS) $(FOCALIZE_BIN_OBJS)
install::
	@$(MKDIR) $(LIB_INSTALL_DIR) && \
	if [ "$(FOCALIZE_FOS)" != "" ] ; then \
	  echo "Installing (FoCaLize) compiled certified libs." && \
	  $(CP) $(FOCALIZE_FOS) $(LIB_INSTALL_DIR)/ \
	fi && \
	if [ "$(FOCALIZE_FOS_UNSAFE)" != "" ] ; then \
	  echo "Installing (FoCaLize) compiled non-certified libs." && \
	  $(CP) $(FOCALIZE_FOS_UNSAFE) $(LIB_INSTALL_DIR)/ \
	fi && \
	if [ "$(FOCALIZE_BYT_OBJS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BYT_OBJS) $(LIB_INSTALL_DIR)/ \
	fi && \
	if [ "$(FOCALIZE_BYT_OBJS_UNSAFE)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BYT_OBJS_UNSAFE) $(LIB_INSTALL_DIR)/ \
	fi && \
	if [ "$(FOCALIZE_BIN_OBJS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BIN_OBJS) $(LIB_INSTALL_DIR)/ \
	fi && \
	if [ "$(FOCALIZE_BIN_OBJS_UNSAFE)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BIN_OBJS_UNSAFE) $(LIB_INSTALL_DIR)/ \
	fi && \
	if [ "$(FOCALIZE_VOS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_VOS) $(LIB_INSTALL_DIR)/ \
	fi && \
	if [ "$(EXTERNAL_BYT_OBJS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_BYT_OBJS) $(LIB_INSTALL_DIR)/ \
	fi && \
	if [ "$(EXTERNAL_BIN_OBJS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_BIN_OBJS) $(LIB_INSTALL_DIR)/ \
	fi && \
	if [ "$(EXTERNAL_VOS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_VOS) $(LIB_INSTALL_DIR)/ \
	fi

uninstall::
	@($(CD) $(LIB_DIR) && \
	  $(RM) $(LIB_INSTALL_DIR))

clean::
	$(RM) $(FOCALIZE_MLS) $(FOCALIZE_ZVS) $(FOCALIZE_VS) && \
	$(RM) $(FOCALIZE_BYT_OBJS) $(FOCALIZE_BIN_OBJS) $(FOCALIZE_VOS) && \
	$(RM) *.annot *.glob && \
	$(RM) $(BIN_EXE)


build.x: bin
	$(CAML_BIN_COMPILER) -o $@ \
           -I $(STDLIB_DIR) -I $(ALGEBRALIB_DIR) -I $(UTILSLIB_DIR) \
            $(STD_BIN_OBJS) $(EXTERNAL_BIN_OBJS) $(FOCALIZE_BIN_OBJS)
