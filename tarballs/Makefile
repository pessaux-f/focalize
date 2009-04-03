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

# $Id: Makefile,v 1.17 2009-04-03 15:50:36 doligez Exp $

ROOT_DIR = ..

include $(ROOT_DIR)/Makefile.config

all:: $(EXTERNAL_TOOLS_DIRS)

$(ABSOLUTE_CAML_SRC_DIR):
	@for i in $(CAML_TAR_BALLS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $(ROOT_DIR) && $(TARX) $(TAR_BALLS_DIR_NAME)/$$i); \
	  err=$$?; \
	  echo "<-- $$i [$$err]"; \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

$(ABSOLUTE_CAMLP5_SRC_DIR):
	@for i in $(CAMLP5_TAR_BALLS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $(ROOT_DIR) && $(TARX) $(TAR_BALLS_DIR_NAME)/$$i); \
	  err=$$?; \
	  echo "<-- $$i [$$err]"; \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

$(ABSOLUTE_COQ_SRC_DIR):
	@for i in $(COQ_TAR_BALLS); do \
	  ($(CD) $(ROOT_DIR) && $(TARX) $(TAR_BALLS_DIR_NAME)/$$i); \
	  err=$$?; \
	  echo "<-- $$i [$$err]"; \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

patches: patch_camlp5 patch_coq

patch_camlp5: $(CAMLP5_PATCH_TAR_BALL)

patch_coq: $(COQ_PATCH_TAR_BALL)

$(CAMLP5_PATCH_TAR_BALL): $(CAMLP5_SRC_DIR_NAME)
	tar cvzf $(CAMLP5_PATCH_TAR_BALL) $(CAMLP5_SRC_DIR_NAME)

$(COQ_PATCH_TAR_BALL): $(COQ_SRC_DIR_NAME)
	tar cvzf $(COQ_PATCH_TAR_BALL) $(COQ_SRC_DIR_NAME)

include $(ROOT_DIR)/Makefile.common

distclean clean uninstall::
	($(RM) $(EXTERNAL_TOOLS_DIRS))
