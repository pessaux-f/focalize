#**********************************************************************#
#                                                                      #
#                        FoCaL compiler                                #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.11 2008-10-14 07:00:37 weis Exp $

ROOT_DIR = ..

include $(ROOT_DIR)/Makefile.config

CAML_INCLUDES = -I ../basement -I ../parser

BYTOBJS = env.cmo scoping.cmo substExpr.cmo substColl.cmo ast_equal.cmo \
	dep_analysis.cmo param_dep_analysis.cmo infer.cmo

include $(ROOT_DIR)/Makefile.common
