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

# $Id: Makefile,v 1.14 2009-03-02 10:19:30 weis Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

# Fill here to add a new directory for a new contribution
SUB_DIRS =\
  utils
#  voter\
#  automata

include $(ROOT_DIR)/Makefile.common

lib: all

byt bin clean doc install depend::
	@for i in $(SUB_DIRS); do \
	  echo "--> $$i ..." && \
	  ($(CD) $$i && $(MAKE) $@); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" && \
	  case $$err in 0);; *) exit $$err;; esac; \
        done
