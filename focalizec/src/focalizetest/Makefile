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

# $Id: Makefile,v 1.1 2009-10-20 13:47:26 weis Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

CAML_INCLUDES = -I ../basement -I ../parser -I ../typing

LOCAL_BYT_OBJS = directive_lexer.cmo make_depend.cmo

BYT_OBJS =\
	../basement/installation.cmo\
	../basement/configuration.cmo\
	../basement/files.cmo\
	$(LOCAL_BYT_OBJS)

include $(ROOT_DIR)/Makefile.common

EXE_NAME = focalizedep

EXE = $(EXE_NAME)-$(VERSION_NUMBER_FULL)

byt:: $(EXE_NAME).byt

$(EXE_NAME).byt: $(BYT_OBJS)
	$(CAML_BYT) str.cma $(BYT_OBJS) -o $@ && \
	$(LN) $(EXE_NAME).byt $(EXE_NAME)

bin:: $(EXE_NAME).bin

$(EXE_NAME).bin: $(BIN_OBJS)
	$(CAML_BIN) str.cmxa $(BIN_OBJS) -o $@ && \
	$(LN) $(EXE_NAME).bin $(EXE_NAME)

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
	$(RM) $(EXE_NAME).byt $(EXE_NAME).bin $(EXE_NAME)
