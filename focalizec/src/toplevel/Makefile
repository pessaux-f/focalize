# $Id: Makefile,v 1.1 2007-07-19 12:04:24 pessaux Exp $

include ../Makefile.config
CAML_COMP_FLAGS = -I ../../focc/ -I ../basement -I ../parser -I ../typing

BYTOBJS = check_file.cmo


ALLOBJS = ../basement/configuration.cmo \
	../basement/configuration.cmo \
	 ../basement/handy.cmo \
	 ../basement/types.cmo \
	../basement/files.cmo \
	../basement/parsetree_utils.cmo \
	\
	../parser/lexer.cmo \
	../parser/parser.cmo \
	../parser/parse_file.cmo \
	../parser/dump_ptree.cmo \
	../parser/sourcify.cmo \
	../parser/oldsourcify.cmo \
	\
	../typing/env.cmo \
	../typing/infer.cmo \
	\
	$(BYTOBJS)


all:: $(ALLOBJS)
	$(CAMLBYT) $(CAML_COMP_FLAGS) $(ALLOBJS) -o check_file

clean::
	$(RM) *~ *.cm* log check_file
	$(RM) doc/*

doc:: $(BYTOBJS)
	$(CAMLDOC) -html -d doc $(BYTOBJS:.cmo=.ml)

depend:
	$(CAMLDEP) $(CAML_COMP_FLAGS) *.mli *.ml > ./.depend

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
