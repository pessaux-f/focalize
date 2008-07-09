# $Id: Makefile,v 1.3 2008-07-09 15:42:19 blond Exp $

include ../Makefile.config
CAML_COMP_FLAGS = -I ../basement -I ../parser -I ../typing -I ../commoncodegen

COQOBJS = external.vo ast.vo pcm.vo ilnl.vo pcm2ilnl.vo extraction.vo
# ilcaml.vo ilnl2ilcaml.vo 


GENNAME = ccodegen
BYTOBJS = imports.cmo $(GENNAME).cmo convert.cmo

CAMLBYT = ocamlc -g -w A -warn-error Aexz
CAMLBIN = ocamlopt -w A -warn-error Aexz

all:: $(COQOBJS) $(BYTOBJS)

doc:: $(BYTOBJS)
	$(CAMLDOC) $(CAML_COMP_FLAGS) -html -d odoc $(BYTOBJS:.cmo=.ml)

clean::
	$(RM) $(COQOBJS)
	$(RM) *~ *.cm* log
	$(RM) odoc/*

remake::
	$(RM) $(GENNAME).ml*
	make all

depend:
#	coqdep *.v > ./.depend
	$(CAMLDEP) $(CAML_COMP_FLAGS) *.mli *.ml > ./.depend

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cmx .v .vo

.v.vo:
	coqc $<

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
