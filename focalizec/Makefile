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

# $Id: Makefile,v 1.36 2009-02-26 13:53:46 pessaux Exp $

ROOT_DIR = .

include $(ROOT_DIR)/Makefile.config

COMPILER_DIRS = src/basement src/parser src/typing\
  src/docgen src/commoncodegen src/mlcodegen src/coqcodegen\
  src/toplevel src/focalizedep

LIB_DIRS = src/stdlib src/extlib src/contribs

DOC_DIRS = doc_src

SUB_DIRS = $(COMPILER_DIRS) $(LIB_DIRS) $(DOC_DIRS)

COMPILER_INSTALL_DIRS = src/toplevel $(LIB_DIRS)

include $(ROOT_DIR)/Makefile.common

all:: lib

byt bin::
	@for i in $(COMPILER_DIRS); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $@); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
        done

lib:
	@for i in $(LIB_DIRS); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $@); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
        done

odoc:: lib
	@for i in $(COMPILER_DIRS); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $@); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
        done

clean doc docdir depend::
	@for i in $(SUB_DIRS); do \
	  if test -d $$i; then \
	    echo "--> $$i ..." >&2 && \
	    ($(CD) $$i && $(MAKE) $@); \
	    err=$$?; \
	    echo "<-- $$i [$$err]" >&2 && \
	    case $$err in 0);; *) exit $$err;; esac; \
	  fi \
        done

install::
	@for i in $(COMPILER_INSTALL_DIRS) $(DOC_DIRS); do \
	  if test -d $$i; then \
	    echo "--> $$i ..." >&2 && \
	    ($(CD) $$i && $(MAKE) $@); \
	    err=$$?; \
	    echo "<-- $$i [$$err]" >&2 && \
	    case $$err in 0);; *) exit $$err;; esac; \
	  fi \
        done
