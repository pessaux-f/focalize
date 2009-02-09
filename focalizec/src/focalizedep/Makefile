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

# $Id: Makefile,v 1.2 2009-02-09 13:28:47 pessaux Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

CAML_INCLUDES = -I ../basement -I ../parser

LOCAL_BYT_OBJS = directive_lexer.cmo make_depend.cmo

BYT_OBJS = \
	../basement/installation.cmo \
	../basement/configuration.cmo \
	../basement/files.cmo \
	$(LOCAL_BYT_OBJS)

include $(ROOT_DIR)/Makefile.common

byt:: focalizedep.byt

focalizedep.byt: $(BYT_OBJS)
	$(CAML_BYT) str.cma $(BYT_OBJS) -o $@
	$(LN) focalizedep.byt focalizedep

bin:: focalizedep.bin

focalizedep.bin: $(BIN_OBJS)
	$(CAML_BIN) str.cmxa $(BIN_OBJS) -o $@
	$(LN) focalizedep.bin focalizedep

install::
	mkdir -p $(BIN_DIR)
	$(CP) focalizedep.byt focalizedep.bin $(BIN_DIR)/
	$(LN) $(BIN_DIR)/focalizedep.bin $(BIN_DIR)/focalizedep

clean::
	$(RM) focalizedep.byt focalizedep.bin focalizedep
