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

# $Id: Makefile,v 1.23 2009-12-09 12:03:36 weis Exp $

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

# Fill here to add a new directory for a new library
SUB_DIRS = algebra access_control
# polynomials (PW: Commented out since does not compile yet (as of 0.6.0).)

include $(ROOT_DIR)/Makefile.common

lib: all

byt bin::
	@for i in $(SUB_DIRS); do \
	  echo "--> $$i ..." && \
	  ($(CD) $$i && $(MAKE) all); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" && \
	  case $$err in 0);; *) exit $$err;; esac; \
        done

clean::
	@for i in $(SUB_DIRS); do \
	  echo "--> $$i ..." && \
	  ($(CD) $$i && $(MAKE) clean); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" && \
	  case $$err in 0);; *) exit $$err;; esac; \
        done

distclean::
	@for i in $(SUB_DIRS); do \
	  echo "--> $$i ..." && \
	  ($(CD) $$i && $(MAKE) distclean); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" && \
	  case $$err in 0);; *) exit $$err;; esac; \
        done

cleanlib: clean

doc::
	@for i in $(SUB_DIRS); do \
	  echo "--> $$i ..." && \
	  ($(CD) $$i && $(MAKE) doc); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

install::
	@for i in $(SUB_DIRS); do \
	  echo "--> $$i ..." && \
	  ($(CD) $$i && $(MAKE) install); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

uninstall::
	@for i in $(SUB_DIRS); do \
	  echo "--> $$i ..." && \
	  ($(CD) $$i && $(MAKE) uninstall); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

depend::
	@for i in $(SUB_DIRS); do \
	  echo "--> $$i ..." && \
	  ($(CD) $$i && $(MAKE) depend); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done
