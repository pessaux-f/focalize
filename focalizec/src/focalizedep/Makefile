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

CAML_INCLUDES =\
 -I ../basement
CAML_FLAGS = -w A-4-9-50 -warn-error $(WARN_ERROR) -safe-string

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

uninstall:
	$(RM) $(INSTALL_BIN_DIR)/$(EXE_NAME).bin $(INSTALL_BIN_DIR)/$(EXE_NAME)

clean:
	$(RM) directive_lexer.ml *.cm* *.o *.a *.annot *.out *.output *.bin *.byt *~ .*~
	$(RM) $(EXE_NAME).byt $(EXE_NAME).bin $(EXE_NAME)

depend: directive_lexer.ml *.ml
	$(CAMLDEP) $(CAML_INCLUDES) *.mli *.ml > ./.depend

include .depend
