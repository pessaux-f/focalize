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

# $Id: Makefile,v 1.4 2008-10-14 07:00:37 weis Exp $

ROOT_DIR = ../..

TEST_FILES = sets_orders.foc

CHECK_FILE = $(ROOT_DIR)/toplevel/check_file.byt

tests:: all

all:: $(CHECK_FILE)
	for i in $(TEST_FILES); do \
	   $(CHECK_FILE) -c $$i; \
	done

$(CHECK_FILE):
	cd $(ROOT_DIR); $(MAKE) all
