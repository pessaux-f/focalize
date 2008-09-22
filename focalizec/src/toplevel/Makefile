# $Id: Makefile,v 1.33 2008-09-22 14:58:19 weis Exp $

ROOTDIR = ..

include $(ROOTDIR)/Makefile.config

CAML_COMP_FLAGS = -I ../basement -I ../parser -I ../typing \
	-I ../commoncodegen -I ../mlcodegen -I ../coqcodegen

LOCALBYTOBJS = check_file.cmo exc_wrapper.cmo

BYTOBJS = \
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
	$(LOCALBYTOBJS)

include $(ROOTDIR)/Makefile.common

.PHONY: check_file_byt_link check_file_bin_link

byt:: check_file_byt_link

check_file_byt_link: check_file.byt
	$(LN) check_file.byt check_file

check_file.byt: $(BYTOBJS)
	$(CAMLBYT) $(BYTOBJS) -o $@

bin:: check_file_bin_link

check_file_bin_link: check_file.bin
	$(LN) check_file.bin check_file

check_file.bin: $(BINOBJS)
	$(CAMLBIN) $(BINOBJS) -o $@

fodump: fodump.cmo
	$(CAMLBYT) ../basement/installation.cmo \
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

install:: all
	echo "TODO"

clean::
	$(RM) check_file.byt check_file.bin check_file fodump
