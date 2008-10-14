# $Id: Makefile,v 1.5 2008-10-14 00:05:05 weis Exp $

ROOTDIR = ..

include $(ROOTDIR)/Makefile.config

SUBDIRS = algebra
#Should be
#SUBDIRS = algebra access_control

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
	  ($(CD) $$i; $(MAKE) doc) \
	done

depend::
	for i in $(SUBDIRS); do \
	  ($(CD) $$i ; $(MAKE) depend) \
	done

