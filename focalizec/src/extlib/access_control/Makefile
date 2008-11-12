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

# $Id: Makefile,v 1.6 2008-11-12 10:19:04 habib Exp $

# Makefile for access control models

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

include $(ROOT_DIR)/Makefile.common

lib: all

all:: ensembles_finis.fo access_control.fo hru.fo rbac.fo ticket.fo unix.fo \
ensembles_finis.v access_control.v hru.v rbac.v ticket.v unix.v

clean::
	$(RM) *.ml *.mli *.zv *.v *.pfc
