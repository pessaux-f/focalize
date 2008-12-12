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

# $Id: Makefile,v 1.12 2008-12-12 08:01:04 weis Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

CAML_INCLUDES = -I ../basement -I ../parser -I ../typing\
	-I ../commoncodegen

BYT_OBJS = species_record_type_generation.cmo rec_let_gen.cmo\
	species_coq_generation.cmo type_coq_generation.cmo\
	main_coq_generation.cmo

include $(ROOT_DIR)/Makefile.common
