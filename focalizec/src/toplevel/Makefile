# $Id: Makefile,v 1.17 2007-10-29 08:18:36 pessaux Exp $

include ../Makefile.config
CAML_COMP_FLAGS = -I ../../focc/ -I ../basement -I ../parser -I ../typing \
	-I ../mlcodegen

BYTOBJS = check_file.cmo exc_wrapper.cmo
BINOBJS = $(BYTOBJS:.cmo=.cmx)

ALLBYTOBJS = \
	../basement/installation.cmo \
	../basement/configuration.cmo \
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
	\
	../typing/env.cmo \
	../typing/scoping.cmo \
	../typing/substExpr.cmo \
	../typing/substColl.cmo \
	../typing/ast_equal.cmo \
	../typing/dep_analysis.cmo \
	../typing/param_dep_analysis.cmo \
	../typing/infer.cmo \
	\
	../mlcodegen/misc_ml_generation.cmo \
	../mlcodegen/externals_ml_generation.cmo \
        ../mlcodegen/type_ml_generation.cmo \
        ../mlcodegen/base_exprs_ml_generation.cmo \
	../mlcodegen/species_ml_generation.cmo \
	../mlcodegen/main_ml_generation.cmo \
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
