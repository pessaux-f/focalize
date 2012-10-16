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

# $Id: Makefile,v 1.79 2012-10-16 11:47:17 pessaux Exp $

ROOT_DIR=.

SUB_DIRS= zenon zvtov focalizec

# No need to include Makefile.rules and .config_var (this latter not
# existing from a scratch checkout). So no need to include the whole
# stuff via Makefile.common. Just Makefile.utils is sufficient.
include $(ROOT_DIR)/Makefile.utils

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
		$(MAKE) depend &&\
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
