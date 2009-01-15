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

# $Id: Makefile,v 1.30 2009-01-15 15:01:33 doligez Exp $

ROOT_DIR = .

include $(ROOT_DIR)/Makefile.config

COMPILER_DIRS = src/basement src/parser src/typing\
  src/docgen src/commoncodegen src/mlcodegen src/coqcodegen\
  src/toplevel

LIB_DIRS = src/stdlib src/extlib src/contribs

SUB_DIRS = $(COMPILER_DIRS) $(LIB_DIRS)

COMPILER_INSTALL_DIRS = src/toplevel $(LIB_DIRS)

include $(ROOT_DIR)/Makefile.common

all:: lib

byt bin::
	@for i in $(COMPILER_DIRS); do \
	  echo "--> $$i ..."; \
	  $(MAKE) -C $$i $@; \
	  err=$$?; \
	  echo "<-- $$i [$$err]"; \
	  case $$err in 0);; *) exit $$err;; esac; \
        done

lib:
	@for i in $(LIB_DIRS); do \
	  echo "--> $$i ..."; \
	  $(MAKE) -C $$i $@; \
	  err=$$?; \
	  echo "<-- $$i [$$err]"; \
	  case $$err in 0);; *) exit $$err;; esac; \
        done

clean doc depend::
	@for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  $(MAKE) -C $$i $@; \
	  err=$$?; \
	  echo "<-- $$i [$$err]"; \
	  case $$err in 0);; *) exit $$err;; esac; \
        done

install::
	@for i in $(COMPILER_INSTALL_DIRS); do \
	  echo "--> $$i ..."; \
	  $(MAKE) -C $$i $@; \
	  err=$$?; \
	  echo "<-- $$i [$$err]"; \
	  case $$err in 0);; *) exit $$err;; esac; \
        done
