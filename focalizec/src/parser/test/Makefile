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

# $Id: Makefile,v 1.7 2008-12-12 08:01:04 weis Exp $

ROOT_DIR = ../../..

include $(ROOT_DIR)/Makefile.config

TEST_FILES = sets_orders.foc

CHECK_FILE = $(ROOT_DIR)/toplevel/focalizec

include $(ROOT_DIR)/Makefile.common

tests:: all

all::
	for i in $(TEST_FILES); do \
	   $(CHECK_FILE) $$i; \
	done
