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

# $Id: Makefile,v 1.14 2009-04-03 15:50:36 doligez Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

BYT_OBJS =\
  installation.cmo configuration.cmo handy.cmo \
  location.cmo types.cmo \
  files.cmo parsetree_utils.cmo miscHelpers.cmo

include $(ROOT_DIR)/Makefile.common

installation.ml:
	@if test ! -L $@ ; then \
	   echo "Please, run configure first."; \
	fi
