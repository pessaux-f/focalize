# $Id: Makefile,v 1.31.2.3 2008-07-31 09:21:06 blond Exp $

include ../Makefile.config
CAML_COMP_FLAGS = -I ../basement -I ../parser -I ../typing \
	-I ../commoncodegen -I ../mlcodegen -I ../coqcodegen \
	-I ../ccodegen

BYTOBJS = check_file.cmo exc_wrapper.cmo
BINOBJS = $(BYTOBJS:.cmo=.cmx)

ALLBYTOBJS = \
	../basement/installation.cmo \
	../basement/configuration.cmo \
	../basement/handy.cmo \
	../basement/location.cmo \
	../basement/types.cmo \
	../basement/files.cmo \
	../basement/parsetree_utils.cmo \
	../basement/miscHelpers.cmo \
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
	../commoncodegen/visUniverse.cmo \
	../commoncodegen/minEnv.cmo \
	../commoncodegen/abstractions.cmo \
	../commoncodegen/misc_common.cmo \
	../commoncodegen/externals_generation_errs.cmo \
	../commoncodegen/recursion.cmo \
	\
	../mlcodegen/misc_ml_generation.cmo \
        ../mlcodegen/type_ml_generation.cmo \
        ../mlcodegen/base_exprs_ml_generation.cmo \
	../mlcodegen/species_ml_generation.cmo \
	../mlcodegen/main_ml_generation.cmo \
	\
	../coqcodegen/species_record_type_generation.cmo \
	../coqcodegen/rec_let_gen.cmo \
	../coqcodegen/species_coq_generation.cmo \
	../coqcodegen/type_coq_generation.cmo \
	../coqcodegen/main_coq_generation.cmo \
	\
	../ccodegen/saving.cmo \
	../ccodegen/ast.cmo \
	../ccodegen/pcm.cmo \
	../ccodegen/cscoping.cmo \
	../ccodegen/compil.cmo \
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

fodump: fodump.cmo
	$(CAMLBYT) $(CAML_COMP_FLAGS) ../basement/installation.cmo \
	../basement/configuration.cmo \
	../basement/handy.cmo \
	../basement/location.cmo \
	../basement/types.cmo \
	../basement/files.cmo \
	../basement/parsetree_utils.cmo \
	../basement/miscHelpers.cmo \
	\
	../parser/lexer.cmo \
	../parser/parser.cmo \
	../parser/parse_file.cmo \
	../parser/dump_ptree.cmo \
	../parser/sourcify.cmo \
	\
	../typing/env.cmo \
	fodump.cmo -o fodump

install: all
	echo "TODO"

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
