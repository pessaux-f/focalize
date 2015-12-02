#**********************************************************************#
#                                                                      #
#                        FoCaLize compiler                             #
#                                                                      #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                                                                      #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.14 2012-10-15 14:41:03 pessaux Exp $

ROOT_DIR=../../..

include $(ROOT_DIR)/Makefile.common

# The list of man files
MAN_FILES= focalizec.1 focalizedep.1 focalizetest.1

.PHONY: all doc clean docdir install

# Nothing to do. Man pages are already written.
all doc:

# Nothing to clean. Man pages are manually written.
clean:

install:
	$(MKDIR) $(INSTALL_MAN_DIR)/man1 &&\
	$(CP) $(MAN_FILES) $(INSTALL_MAN_DIR)/man1/
