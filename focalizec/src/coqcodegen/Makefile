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

# $Id: Makefile,v 1.9 2008-10-14 07:00:37 weis Exp $

ROOTDIR = ..

include $(ROOTDIR)/Makefile.config

CAML_INCLUDES = -I ../basement -I ../parser -I ../typing\
	-I ../commoncodegen

BYTOBJS = species_record_type_generation.cmo rec_let_gen.cmo\
	species_coq_generation.cmo type_coq_generation.cmo\
	main_coq_generation.cmo

include $(ROOTDIR)/Makefile.common
