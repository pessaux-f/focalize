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

# Must be defined before including Makefile.common since is used by it.
BYT_OBJS=\
 pred.cmo foc2spec.cmo focgen.cmo rel_ext.cmo

include $(ROOT_DIR)/Makefile.common

CAML_INCLUDES=\
 -I ../basement -I ../parser -I ../typing\
 -I ../commoncodegen
CAML_FLAGS = -annot -w A-4-9-50 -warn-error $(WARN_ERROR)

depend:
	$(CAMLDEP) $(CAML_INCLUDES) *.mli *.ml > .depend

install: # Nothing to install from there.

uninstall: # Nothing to install from there.

clean:
	$(RM) *.cm* *.o *.a *.annot *.out *.output *.bin *.byt *~ .*~

include .depend
