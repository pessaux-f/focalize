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

# $Id: Makefile,v 1.7 2012-10-15 14:41:49 pessaux Exp $

ROOT_DIR=../../..

SUB_DIRS=refman tutorial

include $(ROOT_DIR)/Makefile.common

all clean depend install doc:
	@for i in $(SUB_DIRS); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $@); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done
