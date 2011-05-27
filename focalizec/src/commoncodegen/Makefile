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

# $Id: Makefile,v 1.15 2011-05-27 14:31:53 weis Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

CAML_INCLUDES = -I ../basement -I ../parser -I ../typing

BYT_OBJS = visUniverse.cmo minEnv.cmo abstractions.cmo misc_common.cmo\
	externals_generation_errs.cmo recursion.cmo

depend-local:
	$(CAML_DEP) *.mli *.ml > ./.depend

include $(ROOT_DIR)/Makefile.common
