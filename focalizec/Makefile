# ************************************************************************** #
#                                                                            #
#                        FoCaLiZe compiler                                   #
#                                                                            #
#            François Pessaux                                                #
#            Pierre Weis                                                     #
#            Damien Doligez                                                  #
#                                                                            #
#                               LIP6  --  INRIA Rocquencourt                 #
#                                                                            #
#  Copyright 2007 - ... LIP6 and INRIA                                       #
#            2012 - ... ENSTA ParisTech                                      #
#  Distributed only by permission.                                           #
#                                                                            #
# ************************************************************************** #


ROOT_DIR=..

include $(ROOT_DIR)/Makefile.common
include $(ROOT_DIR)/focalizec/.config_var

COMPILER_DIRS=\
 src/basement src/parser src/typing\
 src/docgen src/commoncodegen src/mlcodegen src/coqcodegen src/dkcodegen\
 src/testing src/relation_extraction\
 src/toplevel src/focalizedep src/focalizetest src/focalizeig

LIB_DIRS=\
 src/stdlib

DOC_DIRS=\
 doc_src

ALL_DIRS=\
 $(COMPILER_DIRS) $(LIB_DIRS) $(DOC_DIRS)

byt bin:
	@for i in $(COMPILER_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i && $(MAKE) $@);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" >&2 &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done

depend:
	@for i in $(COMPILER_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i && $(MAKE) $@);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" >&2 &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done

lib-depend:
	@for i in $(LIB_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i && $(MAKE) depend);\
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

# Not yet for ocamldoc of the source code.
# odoc: lib
# 	@for i in $(COMPILER_DIRS); do\
# 	  echo "--> $$i ..." >&2 &&\
# 	  ($(CD) $$i && $(MAKE) $@);\
# 	  err=$$?;\
# 	  echo "<-- $$i [$$err]" >&2 &&\
# 	  case $$err in 0);; *) exit $$err;; esac;\
# 	done

clean:
	@for i in $(ALL_DIRS); do\
	  if test -d $$i; then\
	    echo "--> $$i ..." >&2 &&\
	    ($(CD) $$i && $(MAKE) $@);\
	    err=$$?;\
	    echo "<-- $$i [$$err]" >&2 &&\
	    case $$err in 0);; *) exit $$err;; esac;\
	  fi;\
	done

install:
	@for i in $(ALL_DIRS); do\
	  if test -d $$i; then\
	    echo "--> $$i ..." >&2 &&\
	    ($(CD) $$i && $(SUDO) $(MAKE) $@);\
	    err=$$?;\
	    echo "<-- $$i [$$err]" >&2 &&\
	    case $$err in 0);; *) exit $$err;; esac;\
	  fi;\
	done

uninstall:
	@for i in $(ALL_DIRS); do\
	  if test -d $$i; then\
	    echo "--> $$i ..." >&2 &&\
	    ($(CD) $$i && $(SUDO) $(MAKE) $@);\
	    err=$$?;\
	    echo "<-- $$i [$$err]" >&2 &&\
	    case $$err in 0);; *) exit $$err;; esac;\
	  fi;\
	done

doc:
	@for i in $(DOC_DIRS); do\
	  if test -d $$i; then\
	    echo "--> $$i ..." >&2 &&\
	    ($(CD) $$i && $(MAKE) $@);\
	    err=$$?;\
	    echo "<-- $$i [$$err]" >&2 &&\
	    case $$err in 0);; *) exit $$err;; esac;\
	  fi;\
	done


# Everything but installation and doc.
world: byt bin lib-depend lib
