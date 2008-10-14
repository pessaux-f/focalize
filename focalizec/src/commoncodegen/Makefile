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

ROOTDIR = ..

include $(ROOTDIR)/Makefile.config

CAML_INCLUDES = -I ../basement -I ../parser -I ../typing

BYTOBJS = visUniverse.cmo minEnv.cmo abstractions.cmo misc_common.cmo\
	externals_generation_errs.cmo recursion.cmo

include $(ROOTDIR)/Makefile.common
