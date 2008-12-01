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

# $Id: Makefile,v 1.24 2008-12-01 18:50:03 weis Exp $

ROOT_DIR = .

include $(ROOT_DIR)/Makefile.config

COMPILER_DIRS = basement parser typing\
  docgen commoncodegen mlcodegen coqcodegen\
  toplevel

LIB_DIRS = stdlib extlib contribs

SUB_DIRS = $(COMPILER_DIRS) $(LIB_DIRS)

COMPILER_INSTALL_DIRS = toplevel stdlib $(LIB_DIRS)

include $(ROOT_DIR)/Makefile.common

all:: lib

byt bin::
	@for i in $(COMPILER_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $@) || exit; \
	  echo "<-- $$i"; \
        done

lib:
	@for i in $(LIB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $@) || exit; \
	  echo "<-- $$i [$$?]"; \
        done

clean doc depend::
	@for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $@) || exit; \
	  echo "<-- $$i [$$?]"; \
        done

install::
	@for i in $(COMPILER_INSTALL_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $@) || exit; \
	  echo "<-- $$i [$$?]"; \
        done
