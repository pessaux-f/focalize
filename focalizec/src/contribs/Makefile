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

# $Id: Makefile,v 1.4 2008-10-14 07:00:37 weis Exp $

ROOTDIR = ..

include $(ROOTDIR)/Makefile.config

# Fill here to add a new directory for a new contribution
SUBDIRS = 

include $(ROOTDIR)/Makefile.common

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
	  ($(CD) $$i; $(MAKE) install) \
	done

depend::
	for i in $(SUBDIRS); do \
	  ($(CD) $$i ; $(MAKE) depend) \
	done

