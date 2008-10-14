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

# $Id: Makefile,v 1.7 2008-10-14 07:05:56 weis Exp $

ROOT_DIR = ..

include $(ROOT_DIR)/Makefile.config

SUBDIRS = algebra
#Should be
#SUBDIRS = algebra access_control

include $(ROOT_DIR)/Makefile.common

lib: all

byt bin::
	for i in $(SUBDIRS); do \
	  echo "$$i ..."; \
	  ($(CD) $$i; $(MAKE) all) \
        done

clean::
	for i in $(SUBDIRS); do \
	  ($(CD) $$i; $(MAKE) clean) \
        done

doc::
	for i in $(SUBDIRS); do \
	  ($(CD) $$i; $(MAKE) doc) \
	done

install::
	for i in $(SUBDIRS); do \
	  ($(CD) $$i; $(MAKE) doc) \
	done

depend::
	for i in $(SUBDIRS); do \
	  ($(CD) $$i ; $(MAKE) depend) \
	done

