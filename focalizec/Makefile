# $Id: Makefile,v 1.13 2008-09-18 23:15:35 weis Exp $

ROOTDIR = .

include $(ROOTDIR)/Makefile.config

SUBDIRS = basement parser typing\
  commoncodegen mlcodegen coqcodegen\
  toplevel stdlib\
  extlib contribs

include $(ROOTDIR)/Makefile.common

byt::
	for i in $(SUBDIRS); do \
	  echo "$$i ..."; \
	  ($(CD) $$i; $(MAKE) byt) \
        done

bin::
	for i in $(SUBDIRS); do \
	  echo "$$i ..."; \
	  ($(CD) $$i; $(MAKE) bin) \
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

