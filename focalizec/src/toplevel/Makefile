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

# $Id: Makefile,v 1.56 2011-02-28 13:45:51 tollitte Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

CAML_INCLUDES = -I ../basement -I ../parser -I ../typing\
	-I ../commoncodegen -I ../docgen -I ../mlcodegen -I ../coqcodegen

LOCAL_BYT_OBJS = focalizec.cmo exc_wrapper.cmo

BYT_OBJS =\
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
  ../typing/match_analysis.cmo \
	\
	../docgen/doc_lexer.cmo \
	../docgen/utils_docgen.cmo \
	../docgen/env_docgen.cmo \
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
	../coqcodegen/type_coq_generation.cmo \
	../coqcodegen/species_coq_generation.cmo \
	../coqcodegen/main_coq_generation.cmo \
	$(LOCAL_BYT_OBJS)

EXE_NAME = $(PROJECT_NAME)
EXE = $(EXE_NAME)-$(VERSION_NUMBER_FULL)

include $(ROOT_DIR)/Makefile.common

byt:: $(EXE_NAME).byt

$(EXE_NAME).byt: $(BYT_OBJS)
	$(CAML_BYT) str.cma $(BYT_OBJS) -o $@ && \
	$(LN) $(EXE_NAME).byt $(EXE_NAME)

bin:: $(EXE_NAME).bin

$(EXE_NAME).bin: $(BIN_OBJS)
	$(CAML_BIN) str.cmxa $(BIN_OBJS) -o $@ && \
	$(LN) $(EXE_NAME).bin $(EXE_NAME)

fodump: fodump.cmo
	$(CAML_BYT) \
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
	fodump.cmo -o fodump

install::
	$(MKDIR) $(BIN_DIR) && \
	$(CP) $(EXE_NAME).bin $(BIN_DIR)/$(EXE) && \
	($(CD) $(BIN_DIR) && \
	 $(RM) $(EXE_NAME) && \
	 $(LN) $(EXE) $(EXE_NAME))

uninstall::
	($(CD) $(BIN_DIR) && \
	 $(RM) $(EXE) $(EXE_NAME))

clean::
	$(RM) $(EXE_NAME).byt $(EXE_NAME).bin $(EXE_NAME) fodump
