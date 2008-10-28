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

# $Id: Makefile,v 1.4 2008-10-28 15:53:50 weis Exp $

# Makefile for access control models

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

include $(ROOT_DIR)/Makefile.common

lib: all

all:: access_control.fo hru.fo rbac.fo ticket.fo unix.fo

clean::
	$(RM) *.ml *.mli *.zv *.v *.pfc
