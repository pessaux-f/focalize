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

# $Id: Makefile,v 1.16 2009-01-11 02:31:51 weis Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

# Fill here to add a new directory for a new library
SUB_DIRS = algebra access_control

include $(ROOT_DIR)/Makefile.common

lib: all

byt bin::
	for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) all) || exit; \
	  echo "<-- $$i"; \
        done

clean::
	for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) clean) || exit; \
	  echo "<-- $$i"; \
        done

doc::
	for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) doc) || exit; \
	  echo "<-- $$i"; \
	done

install::
	for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) install) || exit; \
	  echo "<-- $$i"; \
	done

depend::
	for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) depend) || exit; \
	  echo "<-- $$i"; \
	done
