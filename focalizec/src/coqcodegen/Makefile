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

# $Id: Makefile,v 1.16 2012-10-15 14:48:16 pessaux Exp $

ROOT_DIR = ../../..

# Must be defined before including Makefile.common since is used by it.
BYT_OBJS = species_record_type_generation.cmo rec_let_gen.cmo\
	 type_coq_generation.cmo species_coq_generation.cmo \
	main_coq_generation.cmo

include $(ROOT_DIR)/Makefile.common

CAML_INCLUDES = -I ../basement -I ../parser -I ../typing\
	-I ../commoncodegen
CAML_FLAGS = -w A-4 -warn-error $(WARN_ERROR)

depend:
	$(CAMLDEP) $(CAML_INCLUDES) *.mli *.ml > ./.depend

install: # Nothing to install from there.

clean:
	$(RM) *.cm* *.o *.a *.annot *.out *.output *.bin *.byt

include .depend
