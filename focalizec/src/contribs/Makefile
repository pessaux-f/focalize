# $Id: Makefile,v 1.2 2008-09-18 23:10:22 weis Exp $

ROOTDIR = ..

include $(ROOTDIR)/Makefile.config

# Fill here to add a new directory for a new contribution
SUBDIRS = 

include $(ROOTDIR)/Makefile.common

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

