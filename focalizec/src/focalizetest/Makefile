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

LOCAL_BYT_OBJS=\
 fresh_variable.cmo own_basics.cmo whattodo.cmo own_types.cmo\
 own_expr.cmo debug.cmo own_prop.cmo focalize_inter.cmo\
 expr_prolog.cmo context_test.cmo own_xml.cmo lexer.cmo whattodo2.cmo\
 own_parser.cmo useful_parsing.cmo to_strings.cmo print_xml.cmo foc_predef.cmo\
 print_foc.cmo print_fml.cmo rewrite_prop.cmo random_rep.cmo\
 own_prolog.cmo print_prolog.cmo test_prolog.cmo\
 to_prolog.cmo species_harness.cmo test_prop.cmo run.cmo

BYT_OBJS =\
 ../basement/build_stamp.cmo\
 ../basement/installation.cmo\
 ../basement/configuration.cmo\
 ../basement/anti_keyword_conflict.cmo\
 ../basement/files.cmo\
 ../basement/location.cmo\
 ../basement/handy.cmo\
 ../basement/types.cmo\
 ../basement/parsetree_utils.cmo\
 ../parser/sourcify.cmo\
 ../typing/env.cmo\
 $(LOCAL_BYT_OBJS)

include $(ROOT_DIR)/Makefile.common

CAML_INCLUDES=-I ../basement -I ../parser -I ../typing
# [Unsure] Exclude warning 32 (value not used) because there are tons of them.
# When refactoring this code, we must reativate this warning.
CAML_FLAGS = -w A-4-9-50-32 -warn-error $(WARN_ERROR)

EXE_NAME=focalizetest

EXE=$(EXE_NAME)-$(VERSION_NUMBER_FULL)

byt: $(EXE_NAME).byt

$(EXE_NAME).byt: $(BYT_OBJS)
	$(CAMLBYT) str.cma $(BYT_OBJS) -o $@ &&\
	$(LN) $(EXE_NAME).byt $(EXE_NAME)

bin: $(EXE_NAME).bin

$(EXE_NAME).bin: $(BIN_OBJS)
	$(CAMLBIN) str.cmxa $(BIN_OBJS) -o $@ &&\
	$(LN) $(EXE_NAME).bin $(EXE_NAME)

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
	$(RM) $(EXE_NAME).byt $(EXE_NAME).bin $(EXE_NAME)

depend: lexer.ml own_parser.ml *.ml *.mli
	$(CAMLDEP) $(CAML_INCLUDES) *.mli *.ml > ./.depend

include .depend
