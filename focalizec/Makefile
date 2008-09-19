# $Id: Makefile,v 1.12 2008-09-18 22:39:00 weis Exp $

ROOTDIR = .

include $(ROOTDIR)/Makefile.config

SUBDIRS = basement parser typing\
  commoncodegen mlcodegen coqcodegen\
  toplevel stdlib

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
	$(CD) tests ; $(MAKE) clean

doc::
	for i in $(SUBDIRS); do \
	  ($(CD) $$i; $(MAKE) doc) \
	done

install::
	$(CD) toplevel; $(MAKE) install
	$(CD) stdlib; $(MAKE) install

depend::
	for i in $(SUBDIRS); do \
	  ($(CD) $$i ; $(MAKE) depend) \
	done

