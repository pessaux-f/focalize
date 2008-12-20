#**********************************************************************#
#                                                                      #
#                        FoCaLize compiler                             #
#                                                                      #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.8 2008-12-20 17:19:14 ayrault Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

# Fill here to add a new directory for a new contribution
SUB_DIRS = \
	utils \
	voter \
	automata

include $(ROOT_DIR)/Makefile.common

lib: all

byt bin::
	for i in $(SUB_DIRS); do \
	  echo "$$i ..."; \
	  ($(CD) $$i; $(MAKE) all) \
        done

clean::
	for i in $(SUB_DIRS); do \
	  ($(CD) $$i; $(MAKE) clean) \
        done

doc::
	for i in $(SUB_DIRS); do \
	  ($(CD) $$i; $(MAKE) doc) \
	done

install::
	for i in $(SUB_DIRS); do \
	  ($(CD) $$i; $(MAKE) install) \
	done

depend::
	for i in $(SUB_DIRS); do \
	  ($(CD) $$i ; $(MAKE) depend) \
	done
