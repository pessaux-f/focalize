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

# $Id: Makefile,v 1.3 2008-12-03 19:10:57 weis Exp $

ROOT_DIR = ..

include $(ROOT_DIR)/Makefile.config

all:: $(EXTERNAL_TOOLS_DIRS)

../$(CAML_DIR):
	for i in $(CAML_TAR_BALL); do \
	  echo "--> $$i ..."; \
	  ($(CD) $(ROOT_DIR); tar xvzf $(TAR_BALLS_DIR_NAME)/$$i) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

../$(CAMLP5_DIR):
	for i in $(CAMLP5_TAR_BALL); do \
	  echo "--> $$i ..."; \
	  ($(CD) $(ROOT_DIR); tar xvzf $(TAR_BALLS_DIR_NAME)/$$i; \
           sed -e "s|/bin/bash|/bin/sh|" \
		< $(CAMLP5_DIR)/configure \
		> $(CAMLP5_DIR)/.configure; \
	   mv $(CAMLP5_DIR)/.configure $(CAMLP5_DIR)/configure; \
	   chmod +x $(CAMLP5_DIR)/configure) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

../$(COQ_DIR):
	for i in $(COQ_TAR_BALL); do \
	  echo "--> $$i ..."; \
	  ($(CD) $(ROOT_DIR); tar xvzf $(TAR_BALLS_DIR_NAME)/$$i) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

include $(ROOT_DIR)/Makefile.common

clean uninstall::
	($(CD) $(ROOT_DIR); $(RM) $(EXTERNAL_TOOLS_DIR))
