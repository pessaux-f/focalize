#**********************************************************************#
#                                                                      #
#                        FoCaLize compiler                             #
#                                                                      #
#            Fran�ois Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                                                                      #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.3 2009-01-30 10:55:50 weis Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

SUB_DIRS = refman

include $(ROOT_DIR)/Makefile.common

DOCDIR_DIR=$(DOCUMENTATION_DIR)/tex

all clean distclean doc docdir depend::
	@for i in $(SUB_DIRS); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $@); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
        done

distclean:: clean
	$(RM) $(DOCDIR_DIR)

docdir::
	$(MKDIR) $(DOCDIR_DIR)

install::
	@for i in $(SUB_DIRS); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $@); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
        done