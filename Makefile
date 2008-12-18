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

# $Id: Makefile,v 1.11 2008-12-18 06:56:42 weis Exp $

ROOT_DIR = .

include $(ROOT_DIR)/Makefile.config

# Defined in Makefile.config:
# TAR_BALLS_DIR
# EXTERNAL_TOOLS_DIRS
# INTERNAL_TOOLS_DIRS

SUB_DIRS = $(EXTERNAL_TOOLS_DIRS) $(INTERNAL_TOOLS_DIRS)

include $(ROOT_DIR)/Makefile.common

.PHONY: configure_external build_external_tools_sources configure_external_tools
.PHONY: build_external_tools build_internal_tools

all:: configure_external build_internal_tools

#
# External tools
#
configure_external: ./.config_var install_external_tools_sources \
configure_external_tools build_external_tools install_external_tools

./.config_var:
	./configure

install_external_tools_sources: $(EXTERNAL_TOOLS_DIRS)

$(EXTERNAL_TOOLS_DIRS):
	for i in $(TAR_BALLS_DIR); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) all) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

configure_external_tools: $(CAML_DIR)/config/Makefile \
$(CAMLP5_DIR)/config/Makefile $(COQ_DIR)/config/Makefile

# We need to configure, build then install in a row, since
# the caml compiler should be installed to configure camlp5,
# the caml compiler should be up and running to compile camlp5;
# the caml compiler AND camlp5 should be installed to configure coq,
# the caml compiler AND camlp5 should be up and running to compile coq.
# All external tools should be installed and up and running to compile
# and install the FoCaLize internal tools zenon and zvtov;
# all the external and internal tools should be compiled and installed to
# compile the focalizec compiler and its libraries.
$(CAML_DIR)/config/Makefile:
	($(CD) $(CAML_DIR); \
	 ./configure $(CAML_CONFIGURE_OPTIONS); \
	 $(MAKE) $(CAML_MAKE_ALL_TARGET); \
	 $(MAKE) install; \
	)

$(CAMLP5_DIR)/config/Makefile:
	($(CD) $(CAMLP5_DIR); \
	 PATH=$(SHARE_PROJECT_DIR)/bin:$$PATH; \
	 ./configure $(CAMLP5_CONFIGURE_OPTIONS); \
	 $(MAKE) $(CAMLP5_MAKE_ALL_TARGET); \
	 $(MAKE) install; \
	)

$(COQ_DIR)/config/Makefile:
	($(CD) $(COQ_DIR); \
	 PATH=$(SHARE_PROJECT_DIR)/bin:$$PATH; \
	 ./configure $(COQ_CONFIGURE_OPTIONS); \
	 $(MAKE) $(COQ_MAKE_ALL_TARGET); \
	 $(MAKE) install; \
	)

build_external_tools: $(EXTERNAL_TOOLS_EXES)

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

#
# Internal tools
#
build_internal_tools: $(INTERNAL_TOOLS_EXES)

$(INTERNAL_TOOLS_EXES):
	for i in $(INTERNAL_TOOLS_DIRS); do \
	  echo "--> $$i ..."; \
	  PATH=$(SHARE_PROJECT_DIR)/bin:$$PATH; \
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
