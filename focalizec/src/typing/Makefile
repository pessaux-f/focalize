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

# $Id: Makefile,v 1.17 2011-05-27 15:05:43 weis Exp $

ROOT_DIR=../..

include $(ROOT_DIR)/Makefile.config

CAML_INCLUDES=-I ../basement -I ../parser

BYT_OBJS=\
 env.cmo scoping.cmo substExpr.cmo substColl.cmo ast_equal.cmo\
 dep_analysis.cmo param_dep_analysis.cmo\
 infer.cmo match_analysis.cmo

include $(ROOT_DIR)/Makefile.common

depend-local:
	$(CAML_DEP) $(BYT_OBJS:.cmo=.mli) $(BYT_OBJS:.cmo=.ml) > .depend
