#**********************************************************************#
#                                                                      #
#                        FoCaLize compiler                             #
#                                                                      #
#            Matthieu Carlier                                          #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2009 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.10 2012-10-30 11:29:06 pessaux Exp $

ROOT_DIR=../../..

LOCAL_BYT_OBJS=\
 fresh_variable.cmo own_basics.cmo whattodo.cmo own_types.cmo\
 own_expr.cmo debug.cmo own_prop.cmo focalize_inter.cmo\
 expr_prolog.cmo context_test.cmo own_xml.cmo lexer.cmo whattodo2.cmo\
 own_parser.cmo useful_parsing.cmo to_strings.cmo print_xml.cmo foc_predef.cmo\
 print_foc.cmo print_fml.cmo rewrite_prop.cmo random_rep.cmo\
 own_prolog.cmo print_prolog.cmo test_prolog.cmo\
 to_prolog.cmo species_harness.cmo test_prop.cmo run.cmo

# Must be defined before including Makefile.common since is used by it.
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
include $(ROOT_DIR)/focalizec/.install_var

CAML_INCLUDES=-I ../basement -I ../parser -I ../typing
CAML_FLAGS = -w A-4-32 -warn-error $(WARN_ERROR)

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

clean:
	$(RM) *.cm* *.o *.a *.annot *.out *.output *.bin *.byt
	$(RM) $(EXE_NAME).byt $(EXE_NAME).bin $(EXE_NAME)

depend: lexer.ml own_parser.ml *.ml *.mli
	$(CAMLDEP) $(CAML_INCLUDES) *.mli *.ml > ./.depend

include .depend
