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

LOCAL_BYT_OBJS=focalizeig.cmo

BYT_OBJS =\
 ../basement/build_stamp.cmo\
 ../basement/installation.cmo\
 ../basement/configuration.cmo\
 ../basement/build_configuration.cmo\
 ../basement/handy.cmo\
 ../basement/location.cmo\
 ../parser/sourcify.cmo\
 ../basement/anti_keyword_conflict.cmo\
 ../basement/types.cmo\
 ../basement/files.cmo\
 ../basement/parsetree_utils.cmo\
 ../typing/env.cmo\
 $(LOCAL_BYT_OBJS)

CAML_INCLUDES =\
 -I ../basement\
 -I ../parser\
 -I ../typing
CAML_FLAGS = -w A-4 -warn-error $(WARN_ERROR)

EXE_NAME=focalizeig

include $(ROOT_DIR)/Makefile.common
include $(ROOT_DIR)/focalizec/.install_var

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
	$(RM) *.cm* *.o *.a *.annot *.out *.output *.bin *.byt *~ .*~
	$(RM) $(EXE_NAME).byt $(EXE_NAME).bin $(EXE_NAME)

depend:
	$(CAMLDEP) $(CAML_INCLUDES) *.mli *.ml > ./.depend

include .depend
