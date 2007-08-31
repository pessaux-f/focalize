# $Id: Makefile,v 1.11 2007-08-31 13:45:52 pessaux Exp $

include ../Makefile.config
CAML_COMP_FLAGS = -I ../../focc/ -I ../basement -I ../parser -I ../typing

BYTOBJS = check_file.cmo exc_wrapper.cmo
BINOBJS = $(BYTOBJS:.cmo=.cmx)

ALLBYTOBJS = ../basement/configuration.cmo \
	../basement/configuration.cmo \
	../basement/handy.cmo \
	../basement/location.cmo \
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
	../typing/scoping.cmo \
	../typing/substExpr.cmo \
	../typing/substColl.cmo \
	../typing/dep_analysis.cmo \
	../typing/infer.cmo \
	\
	$(BYTOBJS)

ALLBINOBJS = $(ALLBYTOBJS:.cmo=.cmx)

ALLOBJS = $(ALLBYTOBJS) $(ALLBINOBJS)

all:: byt #bin

byt: $(ALLBYTOBJS)
	$(CAMLBYT) $(CAML_COMP_FLAGS) $(ALLBYTOBJS) -o check_file.byt
	ln -sf check_file.byt check_file

bin: $(ALLBINOBJS)
	$(CAMLBIN) $(CAML_COMP_FLAGS) $(ALLBINOBJS) -o check_file.bin
	ln -sf check_file.bin check_file

clean::
	$(RM) *~ *.cm* *.o log check_file.byt check_file.bin check_file
	$(RM) odoc/*

doc:: $(BYTOBJS)
	$(CAMLDOC) $(CAML_COMP_FLAGS) -html -d odoc $(BYTOBJS:.cmo=.ml)

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
