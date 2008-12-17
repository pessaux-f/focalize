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

# $Id: Makefile,v 1.10 2008-12-17 18:10:40 weis Exp $

ROOT_DIR = .

include $(ROOT_DIR)/Makefile.config

# Defined in Makefile.config:
# TAR_BALLS_DIR
# EXTERNAL_TOOLS_DIRS
# INTERNAL_TOOLS_DIRS

SUB_DIRS = $(EXTERNAL_TOOLS_DIRS) $(INTERNAL_TOOLS_DIRS)

include $(ROOT_DIR)/Makefile.common

.PHONY: configure_external build_external_tools_sources configure_external_tools build_external_tools
.PHONY: build_internal_tools

all:: configure_external build_internal_tools

configure_external: ./.config_var build_external_tools_sources \
configure_external_tools build_external_tools install_external_tools

./.config_var:
	./configure

build_external_tools_sources: $(EXTERNAL_TOOLS_DIRS)

configure_external_tools: $(CAML_DIR)/config/Makefile $(CAMLP5_DIR)/config/Makefile $(COQ_DIR)/config/Makefile

$(CAML_DIR)/config/Makefile:
	(cd $(CAML_DIR); \
	 ./configure $(CAML_CONFIGURE_OPTIONS); \
	)

$(CAMLP5_DIR)/config/Makefile:
	(cd $(CAMLP5_DIR); \
	 ./configure $(CAMLP5_CONFIGURE_OPTIONS); \
	)

$(COQ_DIR)/config/Makefile:
	(cd $(COQ_DIR); \
	 ./configure $(COQ_CONFIGURE_OPTIONS); \
	)

build_external_tools: $(EXTERNAL_TOOLS_EXES)

install_external_tools: $(EXTERNAL_TOOLS_EXES)
	(cd $(CAML_DIR); \
	 $(MAKE) install; \
	)
	(cd $(CAMLP5_DIR); \
	 $(MAKE) install; \
	)
	(cd $(COQ_DIR); \
	 $(MAKE) install; \
	)

$(EXTERNAL_TOOLS_DIRS):
	for i in $(TAR_BALLS_DIR); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) all) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

$(EXTERNAL_TOOLS_EXES):
	(cd $(CAML_DIR); \
	 $(MAKE) $(CAML_MAKE_ALL_TARGET); \
	)
	(cd $(CAMLP5_DIR); \
	 $(MAKE) $(CAMLP5_MAKE_ALL_TARGET); \
	)
	(cd $(COQ_DIR); \
	 $(MAKE) $(COQ_MAKE_ALL_TARGET); \
	)

build_internal_tools: $(INTERNAL_TOOLS_EXES)

$(INTERNAL_TOOLS_EXES):
	for i in $(INTERNAL_TOOLS_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) all) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

install uninstall doc depend::
	for i in $(SUB_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) $@) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

install depend::
	for i in $(INTERNAL_TOOLS_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) $@) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

unconfigure:
	$(RM) ./.config_var ./.depend

clean::
	for i in $(TAR_BALLS_DIR) $(INTERNAL_TOOLS_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) $@) || exit; \
	  echo "<-- $$i [$$?]"; \
	done
