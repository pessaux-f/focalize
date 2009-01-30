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

# $Id: Makefile,v 1.1 2009-01-30 09:05:37 weis Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

SUB_DIRS = refman

include $(ROOT_DIR)/Makefile.common

all clean doc docdir depend::
	@for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) $@); \
	  err=$$?; \
	  echo "<-- $$i [$$err]"; \
	  case $$err in 0);; *) exit $$err;; esac; \
        done

install::
	@for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) $@); \
	  err=$$?; \
	  echo "<-- $$i [$$err]"; \
	  case $$err in 0);; *) exit $$err;; esac; \
        done
