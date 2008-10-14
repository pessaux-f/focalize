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

# $Id: Makefile,v 1.16 2008-10-14 07:00:37 weis Exp $

ROOTDIR = .

include $(ROOTDIR)/Makefile.config

COMPILER_DIRS = basement parser typing\
  commoncodegen mlcodegen coqcodegen\
  toplevel

LIB_DIRS = stdlib\
  extlib contribs

SUB_DIRS = $(COMPILER_DIRS) $(LIB_DIRS)

include $(ROOTDIR)/Makefile.common

all:: lib

byt bin::
	for i in $(COMPILER_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $@); \
	  echo "<-- $$i"; \
        done

lib:
	for i in $(LIB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $@); \
	  echo "<-- $$i"; \
        done

clean doc depend::
	for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $@); \
	  echo "<-- $$i"; \
        done

install::
	$(CD) toplevel; $(MAKE) install
	$(CD) stdlib; $(MAKE) install
