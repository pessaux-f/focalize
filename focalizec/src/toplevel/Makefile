#**********************************************************************#
#                                                                      #
#                        FoCaLize compiler                             #
#                                                                      #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.43 2008-12-12 08:01:04 weis Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

CAML_INCLUDES = -I ../basement -I ../parser -I ../typing\
	-I ../commoncodegen -I ../docgen -I ../mlcodegen -I ../coqcodegen

LOCAL_BYT_OBJS = focalizec.cmo exc_wrapper.cmo

BYT_OBJS = \
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
	../docgen/main_docgen.cmo \
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
	$(LOCAL_BYT_OBJS)

include $(ROOT_DIR)/Makefile.common

byt:: focalizec.byt

focalizec.byt: $(BYT_OBJS)
	$(CAML_BYT) $(BYT_OBJS) -o $@
	$(LN) focalizec.byt focalizec

bin:: focalizec.bin

focalizec.bin: $(BIN_OBJS)
	$(CAML_BIN) $(BIN_OBJS) -o $@
	$(LN) focalizec.bin focalizec

fodump: fodump.cmo
	$(CAML_BYT) ../basement/installation.cmo \
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

install::
	$(CP) focalizec.byt focalizec.bin $(BIN_DIR)/
	$(LN) $(BIN_DIR)/focalizec.bin $(BIN_DIR)/focalizec

clean::
	$(RM) focalizec.byt focalizec.bin focalizec fodump
