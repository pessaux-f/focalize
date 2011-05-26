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

# $Id: Makefile,v 1.4 2011-05-26 10:21:32 weis Exp $

ROOT_DIR=../..

include $(ROOT_DIR)/Makefile.config

CAML_INCLUDES=-I ../basement -I ../parser -I ../typing

LOCAL_BYT_OBJS=\
 fresh_variable.cmo own_basics.cmo whattodo.cmo own_types.cmo\
 own_expr.cmo debug.cmo own_prop.cmo focalize_inter.cmo\
 expr_prolog.cmo context_test.cmo own_xml.cmo lexer.cmo whattodo2.cmo\
 own_parser.cmo useful_parsing.cmo to_strings.cmo print_xml.cmo foc_predef.cmo\
 print_foc.cmo print_fml.cmo rewrite_prop.cmo random_rep.cmo\
 own_prolog.cmo print_prolog.cmo test_prolog.cmo\
 to_prolog.cmo species_harness.cmo test_prop.cmo run.cmo

BYT_OBJS =\
 ../basement/installation.cmo\
 ../basement/configuration.cmo\
 ../basement/files.cmo\
 ../basement/location.cmo\
 ../basement/handy.cmo\
 ../basement/types.cmo\
 ../basement/parsetree_utils.cmo\
 ../parser/sourcify.cmo\
 ../typing/env.cmo\
 $(LOCAL_BYT_OBJS)

include $(ROOT_DIR)/Makefile.common

EXE_NAME=focalizetest

EXE=$(EXE_NAME)-$(VERSION_NUMBER_FULL)

byt: $(EXE_NAME).byt

$(EXE_NAME).byt: $(BYT_OBJS)
	$(CAML_BYT) str.cma $(BYT_OBJS) -o $@ &&\
	$(LN) $(EXE_NAME).byt $(EXE_NAME)

bin: $(EXE_NAME).bin

$(EXE_NAME).bin: $(BIN_OBJS)
	$(CAML_BIN) str.cmxa $(BIN_OBJS) -o $@ &&\
	$(LN) $(EXE_NAME).bin $(EXE_NAME)

install:
	$(MKDIR) $(BIN_DIR) &&\
	$(CP) $(EXE_NAME).bin $(BIN_DIR)/$(EXE) &&\
	($(CD) $(BIN_DIR) &&\
	 $(RM) $(EXE_NAME) &&\
	 $(LN) $(EXE) $(EXE_NAME))

uninstall:
	($(CD) $(BIN_DIR) &&\
	 $(RM) $(EXE) $(EXE_NAME))

clean:
	$(RM) $(EXE_NAME).byt $(EXE_NAME).bin $(EXE_NAME)
