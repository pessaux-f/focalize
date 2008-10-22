#**********************************************************************#
#                                                                      #
#                        FoCaL compiler                                #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.19 2008-10-22 11:51:04 doligez Exp $

ROOT_DIR = .

include $(ROOT_DIR)/Makefile.config

COMPILER_DIRS = basement parser typing\
  docgen commoncodegen mlcodegen coqcodegen\
  toplevel

LIB_DIRS = stdlib\
  extlib contribs

SUB_DIRS = $(COMPILER_DIRS) $(LIB_DIRS)

include $(ROOT_DIR)/Makefile.common

all:: lib

byt bin::
	for i in $(COMPILER_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $@) || exit; \
	  echo "<-- $$i"; \
        done

lib:
	for i in $(LIB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $@) || exit; \
	  echo "<-- $$i [$$?]"; \
        done

clean doc depend::
	for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $@) || exit; \
	  echo "<-- $$i [$$?]"; \
        done

install::
	$(CD) toplevel; $(MAKE) install
	$(CD) stdlib; $(MAKE) install
