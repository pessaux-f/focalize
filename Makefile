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

# $Id: Makefile,v 1.1 2008-11-30 21:47:14 weis Exp $

ROOT_DIR = `pwd`

include $(ROOT_DIR)/Makefile.config

FOCALIZE_TAR_BALLS = tarballs

TOOLS_DIRS = ocaml camlp5 coq zenon zvtov

COMPILER_DIR = focalizec

SUB_DIRS = $(TOOLS_DIRS) $(COMPILER_DIR)

include $(ROOT_DIR)/Makefile.common

all:: srcs tools compiler

srcs:
	for i in $(FOCALIZE_TAR_BALLS); do \
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
	for i in $(TOOLS_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) all) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

install uninstall clean doc depend::
	for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $@) || exit; \
	  echo "<-- $$i [$$?]"; \
	done
