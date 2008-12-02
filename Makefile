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

# $Id: Makefile,v 1.4 2008-12-02 06:32:16 weis Exp $

ROOT_DIR = .

include $(ROOT_DIR)/Makefile.config

# Defined in Makefile.config:
# TAR_BALLS_DIR
# EXTERNAL_TOOLS_DIRS

COMPILER_DIR = focalizec

SUB_DIRS = $(EXTERNAL_TOOLS_DIRS) $(COMPILER_DIR)

include $(ROOT_DIR)/Makefile.common

.PHONY: srcs compiler tools

all:: srcs tools compiler

srcs:
	for i in $(TAR_BALLS_DIR); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) all) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

compiler:
	for i in $(COMPILER_DIR); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) all) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

tools:
	(cd $(CAML_DIR); \
	 ./configure $(CAML_CONFIGURE_OPTIONS); \
	 $(MAKE) $(CAML_MAKE_ALL_TARGET); \
	 $(MAKE) install; \
	)
	(cd $(CAMLP5_DIR); \
	 ./configure $(CAMLP5_CONFIGURE_OPTIONS); \
	 $(MAKE) $(CAMLP5_MAKE_ALL_TARGET); \
	 $(MAKE) install; \
	)
	(cd $(COQ_DIR); \
	 ./configure $(COQ_CONFIGURE_OPTIONS); \
	 $(MAKE) $(COQ_MAKE_ALL_TARGET); \
	 $(MAKE) install; \
	)

install uninstall clean doc depend::
	for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $@) || exit; \
	  echo "<-- $$i [$$?]"; \
	done
