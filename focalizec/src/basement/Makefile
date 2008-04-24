# $Id: Makefile,v 1.6 2008-04-24 13:30:41 pessaux Exp $

include ../Makefile.config
CAML_COMP_FLAGS =

BYTOBJS = installation.cmo configuration.cmo handy.cmo location.cmo types.cmo \
	files.cmo parsetree_utils.cmo miscHelpers.cmo


all:: $(BYTOBJS)

doc:: $(BYTOBJS)
	$(CAMLDOC) -html -d odoc $(BYTOBJS:.cmo=.ml)

clean::
	$(RM) *~ *.cm* log
	$(RM) odoc/*

depend:
	$(CAMLDEP) $(CAML_COMP_FLAGS) *.mli *.ml > ./.depend


installation.ml:
	@if test ! -L $@ ; then \
		echo "Please, run configure first." ; \
	fi

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cmx 

.ml.cmo:
	$(CAMLBYT) $(CAML_COMP_FLAGS) -c $<

.mli.cmi:
	$(CAMLBYT) $(CAML_COMP_FLAGS) -c $<

.ml.cmx:
	$(CAMLBIN) $(CAML_COMP_FLAGS) -c $<

.mly.ml:
	$(CAMLYACC) $<

.mll.ml:
	$(CAMLLEX) $<

include .depend
