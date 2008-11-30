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

# $Id: Makefile,v 1.1 2008-11-30 21:48:10 weis Exp $

ROOT_DIR = ..

include $(ROOT_DIR)/Makefile.config

all:: install

install:
	for i in $(EXTERNAL_TOOLS_TAR_BALLS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $(ROOT_DIR); tar xvzf tarballs/$$i) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

include $(ROOT_DIR)/Makefile.common

clean uninstall::
	($(CD) $(ROOT_DIR); $(RM) $(EXTERNAL_TOOLS_DIR))
