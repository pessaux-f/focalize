#**********************************************************************#
#                                                                      #
#                        FoCaLiZe compiler                             #
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

# $Id: Makefile,v 1.77 2012-10-15 14:23:06 pessaux Exp $

ROOT_DIR=.

SUB_DIRS= zenon zvtov focalizec

include $(ROOT_DIR)/Makefile.common

all:
	@echo "Toplevel configuration..."
	@./configure
	@echo "Building Zenon..."
	@($(CD) zenon &&\
		./configure &&\
		$(MAKE) all &&\
		$(MAKE) doc &&\
	  echo "Installing..." &&\
		$(SUDO) $(MAKE) install);\
	  err=$$?;\
	  case $$err in 0);; *) exit $$err;; esac;
	@echo "Building Zvtov..."
	@($(CD) zvtov &&\
		./configure &&\
		$(MAKE) all &&\
		$(MAKE) doc &&\
	  echo "Installing..." &&\
		$(SUDO) $(MAKE) install);\
	  err=$$?;\
	  case $$err in 0);; *) exit $$err;; esac;
	@echo "Building FoCaLiZeC and stuff..."
	@($(CD) focalizec &&\
		./configure &&\
		$(MAKE) world &&\
		$(MAKE) doc &&\
	  echo "Installing..." &&\
		$(SUDO) $(MAKE) install);\
	  err=$$?;\
	  case $$err in 0);; *) exit $$err;; esac;

clean:
	@for i in $(SUB_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i && $(MAKE) $@);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" >&2 &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done
