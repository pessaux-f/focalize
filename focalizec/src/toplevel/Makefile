#**********************************************************************#
#                                                                      #
#                        FoCaL compiler                                #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.35 2008-10-14 07:00:37 weis Exp $

ROOTDIR = ..

include $(ROOTDIR)/Makefile.config

CAML_INCLUDES = -I ../basement -I ../parser -I ../typing \
	-I ../commoncodegen -I ../mlcodegen -I ../coqcodegen

LOCAL_BYTOBJS = focalizec.cmo exc_wrapper.cmo

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
	$(LOCAL_BYTOBJS)

include $(ROOTDIR)/Makefile.common

.PHONY: focalizec_byt_link focalizec_bin_link focalizec.byt focalizec.bin fodump

byt:: focalizec_byt_link

focalizec_byt_link: focalizec.byt
	$(LN) focalizec.byt focalizec

focalizec.byt: $(BYTOBJS)
	$(CAMLBYT) $(BYTOBJS) -o $@

bin:: focalizec_bin_link

focalizec_bin_link: focalizec.bin
	$(LN) focalizec.bin focalizec

focalizec.bin: $(BINOBJS)
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
	$(RM) focalizec.byt focalizec.bin focalizec fodump
