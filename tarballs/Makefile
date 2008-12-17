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

# $Id: Makefile,v 1.6 2008-12-17 18:10:40 weis Exp $

ROOT_DIR = ..

include $(ROOT_DIR)/Makefile.config

all:: $(EXTERNAL_TOOLS_DIRS)

$(CAML_DIR):
	for i in $(CAML_TAR_BALL); do \
	  echo "--> $$i ..."; \
	  ($(CD) $(ROOT_DIR); $(TARX) $(TAR_BALLS_DIR_NAME)/$$i) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

$(CAMLP5_DIR):
	for i in $(CAMLP5_TAR_BALL); do \
	  echo "--> $$i ..."; \
	  ($(CD) $(ROOT_DIR); $(TARX) $(TAR_BALLS_DIR_NAME)/$$i; \
           sed -e "s|/bin/bash|/bin/sh|" \
		< $(CAMLP5_DIR_NAME)/configure \
		> $(CAMLP5_DIR_NAME)/.configure; \
	   $(MV) $(CAMLP5_DIR_NAME)/.configure $(CAMLP5_DIR_NAME)/configure; \
	   $(CHMODRX) $(CAMLP5_DIR_NAME)/configure) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

$(COQ_DIR):
	for i in $(COQ_TAR_BALL); do \
	  echo "--> $$i ..."; \
	  ($(CD) $(ROOT_DIR); $(TARX) $(TAR_BALLS_DIR_NAME)/$$i) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

include $(ROOT_DIR)/Makefile.common

clean uninstall::
	($(RM) $(EXTERNAL_TOOLS_DIRS))
