# $Id: Makefile,v 1.14 2008-09-22 14:58:19 weis Exp $

ROOTDIR = .

include $(ROOTDIR)/Makefile.config

SUBDIRS = basement parser typing\
  commoncodegen mlcodegen coqcodegen\
  toplevel stdlib\
  extlib contribs

include $(ROOTDIR)/Makefile.common

byt bin clean doc depend::
	for i in $(SUBDIRS); do \
	  echo "$$i ..."; \
	  ($(CD) $$i; $(MAKE) $@); \
        done

install::
	$(CD) toplevel; $(MAKE) install
	$(CD) stdlib; $(MAKE) install
