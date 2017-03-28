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
#  Copyright 2007 - 2012 LIP6 and INRIA                                      #
#            2012 ENSTA ParisTech                                            #
#  Distributed only by permission.                                           #
#                                                                            #
# ************************************************************************** #


ROOT_DIR=../../..

# Must be defined before including Makefile.common since is used by it.
BYT_OBJS=\
  env.cmo scoping.cmo substExpr.cmo substColl.cmo ast_equal.cmo\
  dep_analysis.cmo param_dep_analysis.cmo infoOutput.cmo\
  visUniverse.cmo minEnv.cmo abstrs.cmo infer.cmo \
  match_analysis.cmo

include $(ROOT_DIR)/Makefile.common

CAML_INCLUDES=-I ../basement -I ../parser
CAML_FLAGS = -w A-4-9-50 -warn-error $(WARN_ERROR) -safe-string

depend:
	$(CAMLDEP) $(CAML_INCLUDES) *.mli *.ml > .depend

install: # Nothing to install from there.

uninstall: # Nothing to install from there.

clean:
	$(RM) *.cm* *.o *.a *.annot *.out *.output *.bin *.byt *~ .*~

include .depend
