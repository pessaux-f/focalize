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

# $Id: Makefile,v 1.3 2008-10-14 07:00:37 weis Exp $

# Makefile for access control models

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

CHECK_FILE = $(ROOT_DIR)/toplevel/check_file.byt
FOCC = $(CHECK_FILE)

include $(ROOT_DIR)/Makefile.common

all:: access_control.fo hru.fo rbac.fo ticket.fo unix.fo

clean::
	$(RM) *.ml *.mli *.zv *.v *.pfc

.PRECIOUS: *.mli *.fo

$(CHECK_FILE):
	cd $(ROOT_DIR); $(MAKE) all
