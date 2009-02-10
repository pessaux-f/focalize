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

# $Id: Makefile,v 1.49 2009-02-10 09:01:35 weis Exp $

ROOT_DIR = .

include $(ROOT_DIR)/Makefile.config

# Defined in Makefile.config:
# TAR_BALLS_DIR
# EXTERNAL_TOOLS_DIRS
# INTERNAL_TOOLS_DIRS

ALL_SUB_DIRS = $(EXTERNAL_TOOLS_DIRS) $(INTERNAL_TOOLS_DIRS)

DOCDIR_DIR = $(DOCUMENTATION_DIR)

include $(ROOT_DIR)/Makefile.common

.PHONY: build_internal_tools clean_zenon clean_zvtov clean_focalizec clean_internal_tools
.PHONY: build_external_tools clean_external_tools

all:: build_external_tools build_internal_tools

# The ./configure make file target for external tools.
.PHONY: configure_external_tools
configure_external_tools: .done_install_external_tools_sources .done_build_external_tools

.PHONY: magic_configure_external_tools
magic_configure_external_tools: \
 .done_magic_install_external_tools_sources .done_magic_build_external_tools

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

.done_magic_install_external_tools_sources: \
  .done_magic_install_external_$(COQ_NAME)_tool_sources
	$(TOUCH) .done_install_external_tools_sources
	$(TOUCH) .done_magic_install_external_tools_sources

.done_install_external_$(CAML_NAME)_tool_sources:
	for i in $(TAR_BALLS_DIR); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $(ABSOLUTE_CAML_SRC_DIR)); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done; \
	$(TOUCH) .done_install_external_$(CAML_NAME)_tool_sources

.done_magic_install_external_$(CAML_NAME)_tool_sources:
	$(TOUCH) .done_install_external_$(CAML_NAME)_tool_sources
	$(TOUCH) .done_magic_install_external_$(CAML_NAME)_tool_sources

.done_install_external_$(CAMLP5_NAME)_tool_sources: \
  .done_install_external_$(CAML_NAME)_tool_sources
	for i in $(TAR_BALLS_DIR); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $(ABSOLUTE_CAMLP5_SRC_DIR)); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done; \
	$(TOUCH) .done_install_external_$(CAMLP5_NAME)_tool_sources

.done_magic_install_external_$(CAMLP5_NAME)_tool_sources: \
  .done_magic_install_external_$(CAML_NAME)_tool_sources
	$(TOUCH) .done_install_external_$(CAMLP5_NAME)_tool_sources
	$(TOUCH) .done_magic_install_external_$(CAMLP5_NAME)_tool_sources

.done_install_external_$(COQ_NAME)_tool_sources: \
  .done_install_external_$(CAMLP5_NAME)_tool_sources
	for i in $(TAR_BALLS_DIR); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $(ABSOLUTE_COQ_SRC_DIR)); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done; \
	$(TOUCH) .done_install_external_$(COQ_NAME)_tool_sources

.done_magic_install_external_$(COQ_NAME)_tool_sources: \
  .done_magic_install_external_$(CAMLP5_NAME)_tool_sources
	$(TOUCH) .done_install_external_$(COQ_NAME)_tool_sources
	$(TOUCH) .done_magic_install_external_$(COQ_NAME)_tool_sources

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
# Hence:
# - external tools are built and installed at project configuration time,
# - internal tools are built and installed at project compilation time,
# - then the focalizec compiler and its libraries are compiled.
# A new make install invocation will install focalizec and the libraries in the proper
# directories, $PREFIX/bin, $PREFIX/lib, etc.
# The prefix directory $PREFIX is defined at the project configuration time.
.done_build_external_tools: .done_build_external_coq_tool
	$(TOUCH) .done_build_external_tools

.done_magic_build_external_tools: .done_magic_build_external_coq_tool
	$(TOUCH) .done_build_external_tools
	$(TOUCH) .done_magic_build_external_tools

.done_build_external_caml_tool: .done_install_external_$(CAML_NAME)_tool_sources
	for i in $(ABSOLUTE_CAML_SRC_DIR); do \
	  echo "--> $$i..." >&2 && \
	  ($(CD) $(ABSOLUTE_CAML_SRC_DIR) && \
	   ./configure $(CAML_CONFIGURE_OPTIONS) && \
	   $(MAKE) $(CAML_MAKE_ALL_TARGET) && \
	   $(MAKE) install); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done; \
	$(TOUCH) .done_build_external_caml_tool

.done_magic_build_external_caml_tool: .done_magic_install_external_$(CAML_NAME)_tool_sources
	$(TOUCH) .done_build_external_caml_tool
	$(TOUCH) .done_magic_build_external_caml_tool

.done_build_external_camlp5_tool: .done_build_external_caml_tool
	for i in $(ABSOLUTE_CAMLP5_SRC_DIR); do \
	  echo "--> $$i..." >&2 && \
	  ($(CD) $(ABSOLUTE_CAMLP5_SRC_DIR) && \
	   PATH=$(TOOLS_PROJECT_DIR)/bin:$$PATH && \
	   ./configure $(CAMLP5_CONFIGURE_OPTIONS) && \
	   $(MAKE) $(CAMLP5_MAKE_ALL_TARGET) && \
	   $(MAKE) install); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done; \
	$(TOUCH) .done_build_external_camlp5_tool

.done_magic_build_external_camlp5_tool: .done_magic_build_external_caml_tool
	$(TOUCH) .done_build_external_camlp5_tool
	$(TOUCH) .done_magic_build_external_camlp5_tool

.done_build_external_coq_tool: .done_build_external_camlp5_tool
	for i in $(ABSOLUTE_COQ_SRC_DIR); do \
	  echo "--> $$i..." >&2 && \
	  ($(CD) $(ABSOLUTE_COQ_SRC_DIR) && \
	   ./configure $(COQ_CONFIGURE_OPTIONS) && \
	   $(COQ_MAKE) $(COQ_MAKE_ALL_TARGET) && \
	   $(COQ_MAKE) install); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done; \
	$(TOUCH) .done_build_external_coq_tool

.done_magic_build_external_coq_tool: .done_magic_build_external_camlp5_tool
	$(TOUCH) .done_build_external_coq_tool
	$(TOUCH) .done_magic_build_external_coq_tool

#
# Internal tools
#
build_internal_tools: .done_build_external_tools .done_build_internal_tools

.done_build_internal_tools: .done_build_focalizec
	$(TOUCH) .done_build_internal_tools

.done_build_zenon $(ZENON_EXES): .done_build_external_tools
	for i in $(ABSOLUTE_ZENON_SRC_DIR); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && ./configure $(ZENON_CONFIGURE_OPTIONS) && \
	   $(MAKE) $(ZENON_MAKE_ALL_tARGET) && \
	   $(MAKE) install); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done; \
	$(TOUCH) .done_build_zenon

.done_build_zvtov $(ZVTOV_EXES): .done_build_zenon
	for i in $(ABSOLUTE_ZVTOV_SRC_DIR); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && ./configure $(ZVTOV_CONFIGURE_OPTIONS) && \
	   $(MAKE) $(ZVTOV_MAKE_ALL_TARGET) && \
           $(MAKE) install); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done; \
	$(TOUCH) .done_build_zvtov

.done_build_focalizec $(FOCALIZEC_EXES): .done_build_zvtov
	for i in $(ABSOLUTE_FOCALIZEC_SRC_DIR); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && ./configure $(FOCALIZEC_CONFIGURE_OPTIONS) && \
	   $(MAKE) $(FOCALIZEC_MAKE_ALL_TARGET)); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done; \
	$(TOUCH) .done_build_focalizec

# To get a correct make all for internal tools we need to have a
# make_and_install target in each internal tool Makefile.
# This target should make all and then install the executable,
# if and only if, either:
# - there is no installed executable yet, or
# - ./executable is newer than the already installed one.
# This may be obtained via a .done_installation hidden file, with explicit
# dependancy ? Or may be a .to_do_installation and .done_installation ?
# or with a shell script that tests the above condition ?
# Another prerequisite is that the ``all'' target indeed does nothing if nothing
# has to be done (not even rebuild the same executable or copy it to a selected
# place).
#
#all:: make_all_internal_tools

#.PHONY make_all_internal_tools
# make_all_internal_tools:
#	for i in $(INTERNAL_TOOLS_DIRS); do \
#	  echo "--> $$i ..."; \
#	  ($(CD) $$i && $(MAKE) $(INTERNAL_TOOLS_MAKE_ALL_TARGET)); \
#	  err=$$?; \
#	  echo "<-- $$i [$$err]"; \
#	  case $$err in 0);; *) exit $$err;; esac; \
#	done; \
#	$(TOUCH) .done_install_focalizec

clean:: clean_internal_tools

install:: .done_build_internal_tools
	for i in $(ABSOLUTE_FOCALIZEC_SRC_DIR); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $@); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done; \
	$(TOUCH) .done_install_focalizec

uninstall doc odoc docdir depend::
	for i in $(INTERNAL_TOOLS_DIRS); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $@); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

distclean::
	$(RM) $(DOCDIR_DIR)

docdir:: doc
	$(MKDIR) $(DOCDIR_DIR) && \
	for i in $(INTERNAL_TOOLS_DIRS); do \
	  echo "--> $$i ..." >&2 && \
	  $(MKDIR) $(DOCDIR_DIR)/$$i && \
	  ($(CD) $$i && $(MAKE) $@) && \
	  if test -d $$i/$(DOCUMENTATION_DIR); then \
	     $(CPR) $$i/$(DOCUMENTATION_DIR)/* $(DOCDIR_DIR)/$$i/; \
	  fi; \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

clean_zenon:
	$(RM) .done_build_internal_tools && \
	for i in $(ZENON_NAME); do \
	  $(RM) .done_build_$$i; \
	  $(TOUCH) $$i/.config_var; \
	done && \
	for i in $(ZENON_NAME); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) clean); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

clean_zvtov:
	$(RM) .done_build_internal_tools && \
	for i in $(ZVTOV_NAME); do \
	  $(RM) .done_build_$$i; \
	  $(TOUCH) $$i/.config_var; \
	done && \
	for i in $(ZVTOV_NAME); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) clean); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

clean_focalizec:
	$(RM) .done_build_internal_tools && \
	for i in $(FOCALIZEC_NAME); do \
	  $(RM) .done_build_$$i; \
	  $(TOUCH) $$i/.config_var; \
	done && \
	for i in $(FOCALIZEC_NAME); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) clean); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

clean_internal_tools: clean_zenon clean_zvtov clean_focalizec

distclean_internal_tools:
	$(RM) .done_build_internal_tools && \
	for i in $(INTERNAL_TOOLS); do \
	  $(RM) .done_build_$$i && \
	  $(TOUCH) $$i/.config_var; \
	done && \
	for i in $(INTERNAL_TOOLS_DIRS); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) distclean); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

clean_external_tools: clean_internal_tools
	$(RM) .done_build_external_tools && \
	for i in $(EXTERNAL_TOOLS); do \
	  $(RM) .done_build_external_$$i_tool; \
	done && \
	for i in $(EXTERNAL_TOOLS_DIRS); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) clean); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

distclean_external_tools: distclean_internal_tools
	$(RM) .done_build_external_tools && \
	for i in $(EXTERNAL_TOOLS); do \
	  $(RM) .done_build_external_$$i_tool; \
	done && \
	for i in $(TAR_BALLS_DIR); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) distclean); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

distclean:: distclean_external_tools unconfigure
