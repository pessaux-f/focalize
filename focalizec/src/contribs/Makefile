# $Id: Makefile,v 1.3 2008-10-13 23:50:56 weis Exp $

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

