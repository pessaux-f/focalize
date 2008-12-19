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

# $Id: Makefile,v 1.19 2008-12-19 10:11:41 weis Exp $

ROOT_DIR = .

include $(ROOT_DIR)/Makefile.config

# Defined in Makefile.config:
# TAR_BALLS_DIR
# EXTERNAL_TOOLS_DIRS
# INTERNAL_TOOLS_DIRS

ALL_SUB_DIRS = $(EXTERNAL_TOOLS_DIRS) $(INTERNAL_TOOLS_DIRS)

#include $(ROOT_DIR)/Makefile.common

.PHONY: configure_external build_external_tools_sources configure_external_tools
.PHONY: build_external_tools build_internal_tools

all:: configure_external build_internal_tools

#
# External tools
#
configure_external: ./.config_var \
 install_external_tools_sources \
 configure_external_tools

./.config_var:
	./configure

install_external_tools_sources: .external_tools_sources_done

.external_tools_sources_done: $(ABSOLUTE_COQ_DIR)
	touch .external_tools_sources_done

$(ABSOLUTE_CAML_DIR):
	for i in $(TAR_BALLS_DIR); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $(ABSOLUTE_CAML_DIR)) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

$(ABSOLUTE_CAMLP5_DIR): $(ABSOLUTE_CAML_DIR)
	for i in $(TAR_BALLS_DIR); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $(ABSOLUTE_CAMLP5_DIR)) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

$(ABSOLUTE_COQ_DIR): $(ABSOLUTE_CAMLP5_DIR)
	for i in $(TAR_BALLS_DIR); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; $(MAKE) $(ABSOLUTE_COQ_DIR)) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

configure_external_tools: .configure_external_tools_done

.configure_external_tools_done: .configure_external_coq_tool_done
	touch .configure_external_tools_done

# We need to configure, build then install in a row, since
# the caml compiler should be installed to configure camlp5,
# the caml compiler should be up and running to compile camlp5;
# the caml compiler AND camlp5 should be installed to configure coq,
# the caml compiler AND camlp5 should be up and running to compile coq.
# All external tools should be installed and up and running to compile
# and install the FoCaLize internal tools zenon and zvtov;
# all the external and internal tools should be compiled and installed to
# compile the focalizec compiler and its libraries.
.configure_external_caml_tool_done: $(ABSOLUTE_CAML_DIR)
	($(CD) $(ABSOLUTE_CAML_DIR); \
	 ./configure $(CAML_CONFIGURE_OPTIONS); \
	 $(MAKE) $(CAML_MAKE_ALL_TARGET); \
	 $(MAKE) install; \
	)
	touch .configure_external_caml_tool_done

 .configure_external_camlp5_tool_done: .configure_external_caml_tool_done
	($(CD) $(ABSOLUTE_CAMLP5_DIR); \
	 PATH=$(SHARE_PROJECT_DIR)/bin:$$PATH; \
	 ./configure $(CAMLP5_CONFIGURE_OPTIONS); \
	 $(MAKE) $(CAMLP5_MAKE_ALL_TARGET); \
	 $(MAKE) install; \
	)
	touch .configure_external_camlp5_tool_done

.configure_external_coq_tool_done: .configure_external_camlp5_tool_done
	($(CD) $(ABSOLUTE_COQ_DIR); \
	 PATH=$(SHARE_PROJECT_DIR)/bin:$$PATH; \
	 ./configure $(COQ_CONFIGURE_OPTIONS); \
	 $(COQ_MAKE) $(COQ_MAKE_ALL_TARGET); \
	 $(COQ_MAKE) install; \
	)
	touch .configure_external_coq_tool_done

#
# Internal tools
#
build_internal_tools: .build_internal_tools_done

.build_internal_tools_done : .configure_external_tools_done .build_focalizec_done
	touch .build_internal_tools_done

.build_zenon_done $(ZENON_EXES): $(ABSOLUTE_COQ_DIR)/config/Makefile
	for i in $(ZENON_DIR); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; \
	   PATH=$(SHARE_PROJECT_DIR)/bin:$$PATH; \
	   $(MAKE) all) || exit; \
	  echo "<-- $$i [$$?]"; \
	done
	touch .build_zenon_done

.build_zvtov_done $(ZVTOV_EXES): .build_zenon_done
	for i in $(ZVTOV_DIR); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; \
	   PATH=$(SHARE_PROJECT_DIR)/bin:$$PATH; \
	   $(MAKE) all) || exit; \
	  echo "<-- $$i [$$?]"; \
	done
	touch .build_zvtov_done

.build_focalizec_done $(FOCALIZEC_EXES): .build_zvtov_done
	for i in $(FOCALIZEC_DIR); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i; \
	   PATH=$(SHARE_PROJECT_DIR)/bin:$$PATH; \
	   $(MAKE) all) || exit; \
	  echo "<-- $$i [$$?]"; \
	done
	touch .build_focalizec_done

install:: .build_internal_tools_done

install uninstall doc depend::
	for i in $(INTERNAL_TOOLS_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) $@) || exit; \
	  echo "<-- $$i [$$?]"; \
	done

install::
	.configure_coq_validation

unconfigure:
	$(RM) ./.config_var ./.depend

clean::
	$(TOUCH) zenon/.config_var
	for i in $(TAR_BALLS_DIR) $(INTERNAL_TOOLS_DIRS); do \
	  echo "--> $$i ..."; \
	  ($(CD) $$i && $(MAKE) $@) || exit; \
	  echo "<-- $$i [$$?]"; \
	done
