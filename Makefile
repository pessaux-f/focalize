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

# $Id: Makefile,v 1.32 2008-12-24 13:11:35 weis Exp $

ROOT_DIR = .

include $(ROOT_DIR)/Makefile.config

# Defined in Makefile.config:
# TAR_BALLS_DIR
# EXTERNAL_TOOLS_DIRS
# INTERNAL_TOOLS_DIRS

ALL_SUB_DIRS = $(EXTERNAL_TOOLS_DIRS) $(INTERNAL_TOOLS_DIRS)

#include $(ROOT_DIR)/Makefile.common

.PHONY: build_internal_tools clean_internal_tools
.PHONY: build_external_tools clean_external_tools

all:: build_external_tools build_internal_tools

# The ./configure make file target for external tools.
.PHONY: configure_external_tools
configure_external_tools: .done_install_external_tools_sources .done_build_external_tools

.PHONY: magic_configure_external_tools
magic_configure_external_tools: \
 .magic_done_install_external_tools_sources .magic_done_build_external_tools

#
# External tools building, configuring and installing.
#
build_external_tools: .config_var \
 .done_install_external_tools_sources \
 .done_build_external_tools

.config_var:
	./configure

#
# Installing external tools sources.
#
.PHONY: install_external_tools_sources
install_external_tools_sources: .done_install_external_tools_sources

.done_install_external_tools_sources: .done_install_external_$(COQ_NAME)_tool_sources
	$(TOUCH) .done_install_external_tools_sources

 .magic_done_install_external_tools_sources:
	$(TOUCH) .magic_done_install_external_$(CAML_NAME)_tool_sources
	$(TOUCH) .done_install_external_$(CAML_NAME)_tool_sources
	$(TOUCH) .magic_done_install_external_$(CAMLP5_NAME)_tool_sources
	$(TOUCH) .done_install_external_$(CAMLP5_NAME)_tool_sources
	$(TOUCH) .magic_done_install_external_$(COQ_NAME)_tool_sources
	$(TOUCH) .done_install_external_$(COQ_NAME)_tool_sources
	$(TOUCH) .magic_done_install_external_tools_sources
	$(TOUCH) .done_install_external_tools_sources

.done_install_external_$(CAML_NAME)_tool_sources:
	for i in $(TAR_BALLS_DIR); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $(ABSOLUTE_CAML_SRC_DIR)) || exit; \
	  echo "<-- $$i [$$?]"; \
	done; \
	$(TOUCH) .done_install_external_$(CAML_NAME)_tool_sources

.done_install_external_$(CAMLP5_NAME)_tool_sources: \
  .done_install_external_$(CAML_NAME)_tool_sources
	for i in $(TAR_BALLS_DIR); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $(ABSOLUTE_CAMLP5_SRC_DIR)) || exit; \
	  echo "<-- $$i [$$?]"; \
	done; \
	$(TOUCH) .done_install_external_$(CAMLP5_NAME)_tool_sources

.done_install_external_$(COQ_NAME)_tool_sources: \
  .done_install_external_$(CAMLP5_NAME)_tool_sources
	for i in $(TAR_BALLS_DIR); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $(ABSOLUTE_COQ_SRC_DIR)) || exit; \
	  echo "<-- $$i [$$?]"; \
	done; \
	$(TOUCH) .done_install_external_$(COQ_NAME)_tool_sources

#
# Building external tools.
#
# We need to configure, build then install in a row, since
# - the caml compiler should be installed to configure camlp5,
# - the caml compiler should be up and running to compile camlp5;
# - the caml compiler AND camlp5 should be installed to configure coq,
# - the caml compiler AND camlp5 should be up and running to compile coq.
# - all external tools should be installed and up and running to compile
#   and install the FoCaLize internal tools zenon and zvtov;
# - all the external and internal tools should be compiled and installed to
#   compile the focalizec compiler and its libraries.
.done_build_external_tools: .done_build_external_coq_tool
	$(TOUCH) .done_build_external_tools

.magic_done_build_external_tools: .magic_done_build_external_coq_tool
	$(TOUCH) .magic_done_build_external_tools
	$(TOUCH) .done_build_external_tools

.done_build_external_caml_tool: $(ABSOLUTE_CAML_SRC_DIR)
	($(CD) $(ABSOLUTE_CAML_SRC_DIR); \
	 ./configure $(CAML_CONFIGURE_OPTIONS); \
	 $(MAKE) $(CAML_MAKE_ALL_TARGET); \
	 $(MAKE) install; \
	); \
	$(TOUCH) .done_build_external_caml_tool

.done_build_external_camlp5_tool: .done_build_external_caml_tool
	($(CD) $(ABSOLUTE_CAMLP5_SRC_DIR); \
	 PATH=$(SHARE_PROJECT_DIR)/bin:$$PATH; \
	 ./configure $(CAMLP5_CONFIGURE_OPTIONS); \
	 $(MAKE) $(CAMLP5_MAKE_ALL_TARGET); \
	 $(MAKE) install; \
	); \
	$(TOUCH) .done_build_external_camlp5_tool

.done_build_external_coq_tool: .done_build_external_camlp5_tool
	($(CD) $(ABSOLUTE_COQ_SRC_DIR); \
	 ./configure $(COQ_CONFIGURE_OPTIONS); \
	 $(COQ_MAKE) $(COQ_MAKE_ALL_TARGET); \
	 $(COQ_MAKE) install; \
	); \
	$(TOUCH) .done_build_external_coq_tool

.magic_done_build_external_coq_tool: .magic_done_build_external_camlp5_tool
	$(TOUCH) .magic_done_build_external_coq_tool
	$(TOUCH) .done_build_external_coq_tool

.magic_done_build_external_camlp5_tool: .magic_done_build_external_caml_tool
	$(TOUCH) .magic_done_build_external_camlp5_tool
	$(TOUCH) .done_build_external_camlp5_tool

.magic_done_build_external_caml_tool:
	$(TOUCH) .magic_done_build_external_caml_tool
	$(TOUCH) .done_build_external_caml_tool

#
# Internal tools
#
build_internal_tools: .done_build_internal_tools

.done_build_internal_tools: .done_build_external_tools .done_build_focalizec
	$(TOUCH) .done_build_internal_tools

.done_build_zenon $(ZENON_EXES): .done_build_external_tools
	for i in $(ABSOLUTE_ZENON_SRC_DIR); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; \
	   ./configure $(ZENON_CONFIGURE_OPTIONS); \
	   $(MAKE) $(ZENON_MAKE_ALL_tARGET)) || exit; \
	  echo "<-- $$i [$$?]"; \
	done; \
	$(TOUCH) .done_build_zenon

.done_build_zvtov $(ZVTOV_EXES): .done_build_zenon
	for i in $(ABSOLUTE_ZVTOV_SRC_DIR); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; \
	   ./configure $(ZVTOV_CONFIGURE_OPTIONS); \
	   $(MAKE) $(ZVTOV_MAKE_ALL_TARGET)) || exit; \
	  echo "<-- $$i [$$?]"; \
	done; \
	$(TOUCH) .done_build_zvtov

.done_build_focalizec $(FOCALIZEC_EXES): .done_build_zvtov
	for i in $(ABSOLUTE_FOCALIZEC_SRC_DIR); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; \
	   ./configure $(FOCALIZEC_CONFIGURE_OPTIONS); \
	   $(MAKE) $(FOCALIZEC_MAKE_ALL_TARGET)) || exit; \
	  echo "<-- $$i [$$?]"; \
	done; \
	$(TOUCH) .done_build_focalizec

install:: .done_build_internal_tools

install uninstall doc depend::
	for i in $(INTERNAL_TOOLS_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) $@) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

unconfigure:
	$(RM) .config_var .depend .done_*

clean_internals:
	for i in $(INTERNAL_TOOLS_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) clean) || exit; \
	  echo "<-- $$i [$$?]"; \
	done; \
	for i in $(INTERNAL_TOOLS); do \
	  $(RM) .done_build_$$i; \
	  $(TOUCH) $$i/.config_var; \
	done

clean_externals:
	for i in $(EXTERNAL_TOOLS_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) clean) || exit; \
	  echo "<-- $$i [$$?]"; \
	done; \
	for i in $(EXTERNAL_TOOLS); do \
	  $(RM) .done_build_external_$$i_tool; \
	done

clean:: clean_internals
	$(RM) .done_*
	for i in $(TAR_BALLS_DIR); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) $@) || exit; \
	  echo "<-- $$i [$$?]"; \
	done
