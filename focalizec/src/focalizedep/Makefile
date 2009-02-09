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

# $Id: Makefile,v 1.1 2009-02-09 10:55:59 pessaux Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

CAML_INCLUDES = -I ../basement -I ../parser

BYT_OBJS = directive_lexer.cmo make_depend.cmo

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
