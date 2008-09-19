# $Id: Makefile,v 1.7 2008-09-18 22:39:00 weis Exp $

ROOTDIR = ..

include $(ROOTDIR)/Makefile.config

BYTOBJS = installation.cmo configuration.cmo handy.cmo location.cmo types.cmo \
	files.cmo parsetree_utils.cmo miscHelpers.cmo

include $(ROOTDIR)/Makefile.common

installation.ml:
	@if test ! -L $@ ; then \
		echo "Please, run configure first." ; \
	fi
