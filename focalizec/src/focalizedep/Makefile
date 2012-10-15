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

# $Id: Makefile,v 1.13 2012-10-15 15:49:42 pessaux Exp $

ROOT_DIR=../../..

LOCAL_BYT_OBJS=\
 directive_lexer.cmo make_depend.cmo

# Must be defined before including Makefile.common since is used by it.
BYT_OBJS=\
 ../basement/build_stamp.cmo\
 ../basement/installation.cmo\
 ../basement/configuration.cmo\
 ../basement/anti_keyword_conflict.cmo\
 ../basement/files.cmo\
 $(LOCAL_BYT_OBJS)

include $(ROOT_DIR)/Makefile.common
include $(ROOT_DIR)/focalizec/.install_var

CAML_INCLUDES =\
 -I ../basement
CAML_FLAGS = -w A -warn-error $(WARN_ERROR)

EXE_NAME=focalizedep

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
	$(RM) *.cm* *.o *.a *.annot *.out *.output *.bin *.byt *~ .*~
	$(RM) $(EXE_NAME).byt $(EXE_NAME).bin $(EXE_NAME)

depend:
	$(CAMLDEP) $(CAML_INCLUDES) *.mli *.ml > ./.depend

include .depend
