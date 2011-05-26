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

# $Id: Makefile,v 1.44 2011-05-26 10:21:32 weis Exp $

ROOT_DIR=.

include $(ROOT_DIR)/Makefile.config

COMPILER_DIRS=\
 src/basement src/parser src/typing\
 src/docgen src/commoncodegen src/mlcodegen src/coqcodegen\
 src/testing src/toplevel src/focalizedep

LIB_DIRS=\
 src/stdlib src/extlib src/contribs

DOC_DIRS=\
 doc_src

SUB_DIRS=\
 $(COMPILER_DIRS) $(LIB_DIRS) $(DOC_DIRS)

COMPILER_INSTALL_DIRS=\
 src/toplevel src/focalizedep src/focalizetest $(LIB_DIRS)

include $(ROOT_DIR)/Makefile.common

# Where we install the libraries.
PROJECT_LIB_INSTALL_DIR=$(LIB_DIR)/$(PROJECT_NAME_FULL)

all-local: lib

byt bin:
	@for i in $(COMPILER_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i && $(MAKE) $@);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" >&2 &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done

lib:
	@for i in $(LIB_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i && $(MAKE) $@);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" >&2 &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done

cleanlib:
	@for i in $(LIB_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i && $(MAKE) $@);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" >&2 &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done

odoc: lib
	@for i in $(COMPILER_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i && $(MAKE) $@);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" >&2 &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done

clean-local distclean doc docdir depend:
	@for i in $(SUB_DIRS); do\
	  if test -d $$i; then\
	    echo "--> $$i ..." >&2 &&\
	    ($(CD) $$i && $(MAKE) $@);\
	    err=$$?;\
	    echo "<-- $$i [$$err]" >&2 &&\
	    case $$err in 0);; *) exit $$err;; esac;\
	  fi\
	done

install uninstall:
	@for i in $(COMPILER_INSTALL_DIRS) $(DOC_DIRS); do\
	  if test -d $$i; then\
	    echo "--> $$i ..." >&2 &&\
	    ($(CD) $$i && $(MAKE) $@);\
	    err=$$?;\
	    echo "<-- $$i [$$err]" >&2 &&\
	    case $$err in 0);; *) exit $$err;; esac;\
	  fi\
	done

install-local:
	($(CD) $(LIB_DIR) &&\
	 $(RM) $(PROJECT_NAME) &&\
	 $(LN) $(PROJECT_NAME_FULL) $(PROJECT_NAME))

uninstall-local:
	$(RM) $(PROJECT_LIB_INSTALL_DIR)

# We need to to finish distclean by unconfiguring this directory;
# so we unconfigure all the SUB_DIRS before unconfiguring here with the
# generic target of Makefile.common.
distclean-local:
	@for i in $(SUB_DIRS); do\
	  if test -d $$i; then\
	    echo "--> $$i ..." >&2 &&\
	    ($(CD) $$i && $(MAKE) unconfigure);\
	    err=$$?;\
	    echo "<-- $$i [$$err]" >&2 &&\
	    case $$err in 0);; *) exit $$err;; esac;\
	  fi\
	done;\
	$(MAKE) unconfigure


test_all-local: tests
	@for i in $(COMPILER_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i && $(MAKE) tests);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" >&2 &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done
