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

# $Id: Makefile,v 1.11 2008-12-12 08:01:04 weis Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

CAML_INCLUDES = -I ../basement -I ../parser -I ../typing -I ../commoncodegen

BYT_OBJS = misc_ml_generation.cmo type_ml_generation.cmo\
	base_exprs_ml_generation.cmo species_ml_generation.cmo\
	main_ml_generation.cmo

include $(ROOT_DIR)/Makefile.common
