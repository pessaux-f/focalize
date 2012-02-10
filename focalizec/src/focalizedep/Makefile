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

# $Id: Makefile,v 1.11 2012-02-10 16:24:40 pessaux Exp $

ROOT_DIR=../..

include $(ROOT_DIR)/Makefile.config

CAML_INCLUDES=-I ../basement -I ../parser

LOCAL_BYT_OBJS=\
 directive_lexer.cmo make_depend.cmo

BYT_OBJS=\
 ../basement/build_stamp.cmo\
 ../basement/installation.cmo\
 ../basement/configuration.cmo\
 ../basement/anti_keyword_conflict.cmo\
 ../basement/files.cmo\
 $(LOCAL_BYT_OBJS)

include $(ROOT_DIR)/Makefile.common

EXE_NAME=focalizedep

EXE=$(EXE_NAME)-$(VERSION_NUMBER_FULL)

byt: $(EXE_NAME).byt

$(EXE_NAME).byt: $(BYT_OBJS)
	$(CAML_BYT) str.cma $(BYT_OBJS) -o $@ &&\
	$(LN) $(EXE_NAME).byt $(EXE_NAME)

bin: $(EXE_NAME).bin

$(EXE_NAME).bin: $(BIN_OBJS)
	$(CAML_BIN) str.cmxa $(BIN_OBJS) -o $@ &&\
	$(LN) $(EXE_NAME).bin $(EXE_NAME)

install-local:
	$(MKDIR) $(BIN_DIR) &&\
	$(CP) $(EXE_NAME).bin $(BIN_DIR)/$(EXE) &&\
	($(CD) $(BIN_DIR) &&\
	 $(RM) $(EXE_NAME) &&\
	 $(LN) $(EXE) $(EXE_NAME))

uninstall-local:
	($(CD) $(BIN_DIR) &&\
	 $(RM) $(EXE) $(EXE_NAME))

clean-local:
	$(RM) $(EXE_NAME).byt $(EXE_NAME).bin $(EXE_NAME)

depend-local:
	$(CAML_DEP) *.mli *.ml > ./.depend
