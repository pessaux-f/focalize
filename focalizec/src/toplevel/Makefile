# ************************************************************************** #
#                                                                            #
#                        FoCaLiZe compiler                                   #
#                                                                            #
#            François Pessaux                                                #
#            Pierre Weis                                                     #
#            Damien Doligez                                                  #
#                                                                            #
#                               LIP6  --  INRIA Rocquencourt                 #
#                                                                            #
#  Copyright 2007 - ... LIP6 and INRIA                                       #
#            2012 - ... ENSTA ParisTech                                      #
#  Distributed only by permission.                                           #
#                                                                            #
# ************************************************************************** #


ROOT_DIR=../../..

LOCAL_BYT_OBJS=focalizec.cmo exc_wrapper.cmo

# Must be defined before including Makefile.common since is used by it.
BYT_OBJS =\
 ../basement/build_stamp.cmo\
 ../basement/installation.cmo\
 ../basement/configuration.cmo\
 ../basement/build_configuration.cmo\
 ../basement/handy.cmo\
 ../basement/location.cmo\
 ../basement/anti_keyword_conflict.cmo\
 ../basement/types.cmo\
 ../basement/files.cmo\
 ../basement/parsetree_utils.cmo\
 ../basement/miscHelpers.cmo\
 \
 ../parser/lexer.cmo\
 ../parser/parser.cmo\
 ../parser/parse_file.cmo\
 ../parser/dump_ptree.cmo\
 ../parser/sourcify.cmo\
 \
 ../typing/env.cmo\
 ../typing/scoping.cmo\
 ../typing/substExpr.cmo\
 ../typing/substColl.cmo\
 ../typing/ast_equal.cmo\
 ../typing/dep_analysis.cmo\
 ../typing/param_dep_analysis.cmo\
 ../typing/infoOutput.cmo\
 ../typing/visUniverse.cmo\
 ../typing/minEnv.cmo\
 ../typing/abstrs.cmo\
 ../typing/infer.cmo\
 ../typing/match_analysis.cmo\
 \
 ../docgen/doc_lexer.cmo\
 ../docgen/utils_docgen.cmo\
 ../docgen/env_docgen.cmo\
 ../docgen/main_docgen.cmo\
 \
 ../commoncodegen/misc_common.cmo\
 ../commoncodegen/externals_generation_errs.cmo\
 ../commoncodegen/recursion.cmo\
 \
 ../mlcodegen/ml_pprint.cmo\
 ../mlcodegen/type_ml_generation.cmo\
 ../mlcodegen/base_exprs_ml_generation.cmo\
 ../mlcodegen/species_ml_generation.cmo\
 ../mlcodegen/main_ml_generation.cmo\
 \
 ../coqcodegen/coq_pprint.cmo\
 ../coqcodegen/species_record_type_coq_generation.cmo\
 ../coqcodegen/rec_let_coq_gen.cmo\
 ../coqcodegen/type_coq_generation.cmo\
 ../coqcodegen/species_coq_generation.cmo\
 ../coqcodegen/main_coq_generation.cmo\
 \
 ../dkcodegen/dk_pprint.cmo\
 ../dkcodegen/expr_dk_generation.cmo\
 ../dkcodegen/species_record_type_dk_generation.cmo\
 ../dkcodegen/rec_let_dk_gen.cmo\
 ../dkcodegen/type_dk_generation.cmo\
 ../dkcodegen/species_dk_generation.cmo\
 ../dkcodegen/main_dk_generation.cmo\
 \
 ../testing/testing.cmo\
\
 ../relation_extraction/pred.cmo\
 ../relation_extraction/foc2spec.cmo\
 ../relation_extraction/focgen.cmo\
 ../relation_extraction/rel_ext.cmo\
\
 $(LOCAL_BYT_OBJS)

FODUMP_OBJS=\
 ../basement/build_stamp.cmo\
 ../basement/installation.cmo\
 ../basement/configuration.cmo\
 ../basement/handy.cmo\
 ../basement/location.cmo\
 ../basement/types.cmo\
 ../basement/anti_keyword_conflict.cmo\
 ../basement/files.cmo\
 ../basement/parsetree_utils.cmo\
 ../basement/miscHelpers.cmo\
 \
 ../parser/lexer.cmo\
 ../parser/parser.cmo\
 ../parser/parse_file.cmo\
 ../parser/dump_ptree.cmo\
 ../parser/sourcify.cmo\
 \
 ../typing/env.cmo\
 fodump.cmo

include $(ROOT_DIR)/Makefile.common
include $(ROOT_DIR)/focalizec/.install_var

CAML_INCLUDES =\
 -I ../basement\
 -I ../parser\
 -I ../typing\
 -I ../commoncodegen\
 -I ../docgen\
 -I ../mlcodegen\
 -I ../coqcodegen\
 -I ../dkcodegen\
 -I ../testing\
 -I ../relation_extraction
CAML_FLAGS = -w A-4 -warn-error $(WARN_ERROR)

EXE_NAME=focalizec

byt: $(EXE_NAME).byt

$(EXE_NAME).byt: $(BYT_OBJS)
	$(CAMLBYT) str.cma $(BYT_OBJS) -o $@ &&\
	$(LN) $(EXE_NAME).byt $(EXE_NAME)

bin: $(EXE_NAME).bin

$(EXE_NAME).bin: $(BIN_OBJS)
	$(CAMLBIN) str.cmxa $(BIN_OBJS) -o $@ &&\
	$(LN) $(EXE_NAME).bin $(EXE_NAME)

fodump: $(FODUMP_OBJS)
	$(CAML_BYT) $(FODUMP_OBJS) -o fodump

install:
	$(MKDIR) $(INSTALL_BIN_DIR) &&\
	$(CP) $(EXE_NAME).bin $(INSTALL_BIN_DIR)/ &&\
	($(CD) $(INSTALL_BIN_DIR) &&\
	 $(RM) $(EXE_NAME) &&\
	 $(LN) $(EXE_NAME).bin $(EXE_NAME))

uninstall:
	$(RM) $(INSTALL_BIN_DIR)/$(EXE_NAME).bin $(INSTALL_BIN_DIR)/$(EXE_NAME)

clean:
	$(RM) *.cm* *.o *.a *.annot *.out *.output *.bin *.byt
	$(RM) $(EXE_NAME).byt $(EXE_NAME).bin $(EXE_NAME) fodump

depend:
	$(CAMLDEP) $(CAML_INCLUDES) *.mli *.ml > .depend

include .depend
