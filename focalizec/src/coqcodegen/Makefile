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

# $Id: Makefile,v 1.14 2011-05-27 14:31:53 weis Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

CAML_INCLUDES = -I ../basement -I ../parser -I ../typing\
	-I ../commoncodegen

BYT_OBJS = species_record_type_generation.cmo rec_let_gen.cmo\
	 type_coq_generation.cmo species_coq_generation.cmo \
	main_coq_generation.cmo

depend-local:
	$(CAML_DEP) *.mli *.ml > ./.depend

include $(ROOT_DIR)/Makefile.common
