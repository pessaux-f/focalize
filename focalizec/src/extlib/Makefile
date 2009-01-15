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

# $Id: Makefile,v 1.17 2009-01-15 15:01:33 doligez Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

# Fill here to add a new directory for a new library
SUB_DIRS = algebra access_control

include $(ROOT_DIR)/Makefile.common

lib: all

byt bin::
	for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  $(MAKE) -C $$i all; \
	  err=$$?; \
	  echo "<-- $$i [$$err]"; \
	  case $$err in 0);; *) exit $$err;; esac; \
        done

clean::
	for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  $(MAKE) -C $$i clean; \
	  err=$$?; \
	  echo "<-- $$i [$$err]"; \
	  case $$err in 0);; *) exit $$err;; esac; \
        done

doc::
	for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) doc) || exit; \
	  err=$$?; \
	  echo "<-- $$i [$$err]"; \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

install::
	for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) install) || exit; \
	  err=$$?; \
	  echo "<-- $$i [$$err]"; \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

depend::
	for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) depend) || exit; \
	  err=$$?; \
	  echo "<-- $$i [$$err]"; \
	  case $$err in 0);; *) exit $$err;; esac; \
	done
