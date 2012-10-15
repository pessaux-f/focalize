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

# $Id: Makefile,v 1.14 2012-10-15 14:50:45 pessaux Exp $

ROOT_DIR = ../../..

# Must be defined before including Makefile.common since is used by it.
BYT_OBJS = misc_ml_generation.cmo type_ml_generation.cmo\
	base_exprs_ml_generation.cmo species_ml_generation.cmo\
	main_ml_generation.cmo

include $(ROOT_DIR)/Makefile.common

CAML_INCLUDES = -I ../basement -I ../parser -I ../typing -I ../commoncodegen
CAML_FLAGS = -w A-4 -warn-error $(WARN_ERROR)

depend:
	$(CAMLDEP) $(CAML_INCLUDES) *.mli *.ml > ./.depend

install: # Nothing to install from there.

clean:
	$(RM) *.cm* *.o *.a *.annot *.out *.output *.bin *.byt

include .depend
