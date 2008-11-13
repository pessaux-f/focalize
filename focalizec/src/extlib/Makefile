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

# $Id: Makefile,v 1.10 2008-11-13 11:05:11 habib Exp $

ROOT_DIR = ..

include $(ROOT_DIR)/Makefile.config

SUB_DIRS = algebra access_control

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
