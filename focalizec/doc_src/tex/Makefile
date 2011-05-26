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

# $Id: Makefile,v 1.6 2011-05-26 10:21:32 weis Exp $

ROOT_DIR=../..

include $(ROOT_DIR)/Makefile.config

SUB_DIRS=refman tutorial

include $(ROOT_DIR)/Makefile.common

DOCDIR_DIR=$(DOC_ROOT_DIR)/tex

all clean-local distclean doc docdir depend:
	@for i in $(SUB_DIRS); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $@); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

distclean-local:
	$(RM) $(DOCDIR_DIR)

docdir-local:
	$(MKDIR) $(DOCDIR_DIR)

install-local uninstall-local:
	@for i in $(SUB_DIRS); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $@); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done
