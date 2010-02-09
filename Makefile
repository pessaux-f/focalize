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

# $Id: Makefile,v 1.72 2010-02-09 21:09:27 weis Exp $

ROOT_DIR = .

include $(ROOT_DIR)/Makefile.config

# Defined in Makefile.config:
#
# TAR_BALLS_DIR = $(ROOT_DIR)/$(TAR_BALLS_DIR_NAME)
## TAR_BALLS_DIR_NAME = tarballs
#
# TOOLS_EXTERNAL_DIRS = \
#  $(ABSOLUTE_CAML_SRC_DIR) $(ABSOLUTE_CAMLP5_SRC_DIR) $(ABSOLUTE_COQ_SRC_DIR)
#
# TOOLS_INTERNAL_DIRS = $(ZENON_SRC_DIR) $(ZVTOV_SRC_DIR)
# DELIVERABLES = $(FOCALIZEC_NAME) $(FOCALIZEDEP_NAME)
# DELIVERABLES_DIRS = $(FOCALIZEC_SRC_DIR) $(FOCALIZEDEP_SRC_DIR)
#
# DOC_ROOT_DIR=$(ROOT_DIR)/doc

ALL_SUB_DIRS = \
 $(TOOLS_EXTERNAL_DIRS) $(TOOLS_INTERNAL_DIRS) $(DELIVERABLES_DIRS)

DOCDIR_DIR = $(DOC_ROOT_DIR)

include $(ROOT_DIR)/Makefile.common

.PHONY: \
  configure configure_external_tools configure_internal_tools configure_deliverables

.PHONY: \
  clean_external_tools clean_internal_tools clean_deliverables

.PHONY: \
  build_external_tools build_internal_tools build_deliverables

.PHONY: \
  install_external_tools install_internal_tools install_deliverables

.PHONY: install_external_tools_sources

#.PHONY: clean_external_tools
.PHONY: clean_caml clean_camlp5 clean_coq

#.PHONY: clean_internal_tools
.PHONY: clean_zenon clean_zvtov

#.PHONY: clean_deliverables
.PHONY: clean_focalizec clean_focalizedep

all:: build_external_tools build_internal_tools build_deliverables

# The ./configure make file target for external tools.
configure: .done_configure
.done_configure: \
  .done_configure_external_tools \
  .done_configure_internal_tools \
  .done_configure_deliverables
	@$(TOUCH) .done_configure

# External tools configuration in fact means installing their sources and
# configuring them.
configure_external_tools: .done_configure_external_tools

.done_configure_external_tools: .done_configure_external_caml_tool
	@$(TOUCH) .done_configure_external_tools

.done_configure_external_coq_tool: .done_configure_external_camlp5_tool

.done_configure_external_camlp5_tool: .done_configure_external_caml_tool

# Configuring Caml
.done_configure_external_caml_tool: \
  .done_install_external_$(CAML_NAME)_tool_sources
	@for i in $(ABSOLUTE_CAML_SRC_DIR); do \
	  echo "--> Configuring $$i..." >&2 && \
	  ($(CD) $(ABSOLUTE_CAML_SRC_DIR) && \
	   ./configure $(CAML_CONFIGURE_OPTIONS)); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done && \
	$(TOUCH) .done_configure_external_caml_tool

# Configuring Camlp5: we cannot perform the configuration before having our
# caml compiler properly installed (see below the building target for camlp5
# .done_build_external_camlp5_tool).
.done_configure_external_camlp5_tool: \
  .done_install_external_$(CAMLP5_NAME)_tool_sources \
  .done_configure_external_caml_tool
	@for i in $(ABSOLUTE_CAMLP5_SRC_DIR); do \
	  echo "--> Configuring $$i..." >&2 && \
	  ($(CD) $(ABSOLUTE_CAMLP5_SRC_DIR)); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done && \
	$(TOUCH) .done_configure_external_camlp5_tool

# Coq
.done_configure_external_coq_tool: \
  .done_install_external_$(COQ_NAME)_tool_sources \
  .done_configure_external_camlp5_tool
	@for i in $(ABSOLUTE_COQ_SRC_DIR); do \
	  echo "--> Configuring $$i..." >&2 && \
	  ($(CD) $(ABSOLUTE_COQ_SRC_DIR) && \
	   ./configure $(COQ_CONFIGURE_OPTIONS)); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done && \
	$(TOUCH) .done_configure_external_coq_tool

# Internal tools configuration
configure_internal_tools: .done_configure_internal_tools

.done_configure_internal_tools: \
  .done_configure_external_tools \
  .done_configure_zenon \
  .done_configure_zvtov
	@$(TOUCH) .done_configure_internal_tools

.done_configure_zenon: .done_configure_external_tools
	@for i in $(ABSOLUTE_ZENON_SRC_DIR); do \
	  echo "--> Configuring $$i ..." >&2 && \
	  ($(CD) $$i && ./configure $(ZENON_CONFIGURE_OPTIONS)); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done && \
	$(TOUCH) .done_configure_zenon

.done_configure_zvtov : .done_configure_zenon
	@for i in $(ABSOLUTE_ZVTOV_SRC_DIR); do \
	  echo "--> Configuring $$i ..." >&2 && \
	  ($(CD) $$i && ./configure $(ZVTOV_CONFIGURE_OPTIONS)); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done && \
	$(TOUCH) .done_configure_zvtov

# Deliverables configuration
configure_deliverables: .done_configure_deliverables

.done_configure_deliverables: \
  .done_configure_internal_tools \
  .done_configure_focalizec \
  .done_configure_focalizedep
	@$(TOUCH) .done_configure_deliverables

.done_configure_focalizec: .done_configure_internal_tools
	@for i in $(ABSOLUTE_FOCALIZEC_SRC_DIR); do \
	  echo "--> Configuring $$i ..." >&2 && \
	  ($(CD) $$i && ./configure $(FOCALIZEC_CONFIGURE_OPTIONS)); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done && \
	$(TOUCH) .done_configure_focalizec

.done_configure_focalizedep: \
  .done_configure_internal_tools .done_configure_focalizec
	@$(TOUCH) .done_configure_focalizedep

# If we have to rebuild .config_var
.config_var:
	@./configure

# Dist cleaning configuration.
# Warning: after running this target, you must run configure again to be able
# to use this Makefile! (Since included file ./.config_var has been removed.)
unconfigure::
	# Just for developpers, but harmless for users.
	@$(RM) .distribution_var

#
# Installing external tools sources.
#
.PHONY: install_external_tools_sources clean_install_external_tools_sources
install_external_tools_sources: .done_install_external_tools_sources

.done_install_external_tools_sources: .done_install_external_$(COQ_NAME)_tool_sources
	@$(TOUCH) .done_install_external_tools_sources

clean_install_external_tools_sources:
	@for i in \
	  $(ABSOLUTE_COQ_SRC_DIR) $(ABSOLUTE_CAMLP5_SRC_DIR) $(ABSOLUTE_CAML_SRC_DIR); do \
	  echo "--> Cleaning $$i ..." >&2 && \
	  $(RM) $$i; \
	done && \
	$(RM) .done_install_external_$(COQ_NAME)_tool_sources  && \
	$(RM) .done_install_external_$(CAMLP5_NAME)_tool_sources  && \
	$(RM) .done_install_external_$(CAML_NAME)_tool_sources && \
	$(RM) .done_install_external_tools_sources

# Caml sources
install_external_$(CAML_NAME)_tool_sources: \
   .done_install_external_$(CAML_NAME)_tool_sources

.done_install_external_$(CAML_NAME)_tool_sources: .config_var
	@for i in $(TAR_BALLS_DIR); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $(ABSOLUTE_CAML_SRC_DIR)); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done && \
	$(TOUCH) .done_install_external_$(CAML_NAME)_tool_sources

# CamlP5 sources
install_external_$(CAMLP5_NAME)_tool_sources: \
  .done_install_external_$(CAMLP5_NAME)_tool_sources

.done_install_external_$(CAMLP5_NAME)_tool_sources: \
   .done_install_external_$(CAML_NAME)_tool_sources
	@for i in $(TAR_BALLS_DIR); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $(ABSOLUTE_CAMLP5_SRC_DIR)); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done && \
	$(TOUCH) .done_install_external_$(CAMLP5_NAME)_tool_sources

# Coq sources
install_external_$(COQ_NAME)_tool_sources: \
  .done_install_external_$(COQ_NAME)_tool_sources

.done_install_external_$(COQ_NAME)_tool_sources: \
  .done_install_external_$(CAMLP5_NAME)_tool_sources
	@for i in $(TAR_BALLS_DIR); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $(ABSOLUTE_COQ_SRC_DIR)); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done && \
	$(TOUCH) .done_install_external_$(COQ_NAME)_tool_sources

#
# Building external tools.
#
# We need to configure, build then install in a row, since
# - the caml compiler should be installed to configure camlp5;
# - the caml compiler should be up and running to compile camlp5;
# - the caml compiler AND camlp5 should be installed to configure coq;
# - the caml compiler AND camlp5 should be up and running to compile coq.
# - all external tools should be installed and up and running to compile
#   and install the FoCaLize internal tools zenon and zvtov;
# - all the external and internal tools should be compiled and installed to
#   compile the focalizec compiler and its libraries.
# Hence:
# - external tools are built and installed at project configuration time,
# - internal tools are built and installed at project compilation time,
# - then the focalizec compiler and its libraries are compiled.
# A new make install invocation will install focalizec and the libraries in
# the proper directories, $PREFIX/bin, $PREFIX/lib, etc.
# The prefix directory $PREFIX is defined at the project configuration time.
build_external_tools: .done_build_external_tools

.done_build_external_tools: \
  .done_configure_external_tools \
  .done_build_external_caml_tool \
  .done_build_external_camlp5_tool \
  .done_build_external_coq_tool
	@$(TOUCH) .done_build_external_tools

# Caml
.done_build_external_caml_tool: \
  .done_install_external_$(CAML_NAME)_tool_sources
	@for i in $(ABSOLUTE_CAML_SRC_DIR); do \
	  echo "--> Building $$i..." >&2 && \
	  ($(CD) $(ABSOLUTE_CAML_SRC_DIR) && \
	   $(MAKE) $(CAML_MAKE_ALL_TARGET) && \
	   $(MAKE) install); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done && \
	$(TOUCH) .done_build_external_caml_tool

# Camlp5: we need to configure here after Caml compiler has been installed,
# since the camlp5 configuration does not provide an option to tell which
# caml compiler we need (it just calls ocamlc -v and ocamlc -where and record
# the answer from whichever caml compiler found in the search path). Too bad
# for us: we really need to install our caml compiler before configuring
# Camlp5, unless we modify completely camlp5 configuration procedure...
.done_build_external_camlp5_tool:\
   .done_build_external_caml_tool \
   .done_configure_external_camlp5_tool
	@for i in $(ABSOLUTE_CAMLP5_SRC_DIR); do \
	  echo "--> Building $$i..." >&2 && \
	  ($(CD) $(ABSOLUTE_CAMLP5_SRC_DIR) && \
	   PATH=$(TOOLS_PROJECT_DIR)/bin:$$PATH && \
	   ./configure $(CAMLP5_CONFIGURE_OPTIONS) && \
	   $(MAKE) $(CAMLP5_MAKE_ALL_TARGET) && \
	   $(MAKE) install); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done && \
	$(TOUCH) .done_build_external_camlp5_tool

# Coq
.done_build_external_coq_tool:\
   .done_build_external_camlp5_tool \
   .done_configure_external_coq_tool
	@for i in $(ABSOLUTE_COQ_SRC_DIR); do \
	  echo "--> Building $$i..." >&2 && \
	  ($(CD) $(ABSOLUTE_COQ_SRC_DIR) && \
	   $(COQ_MAKE) $(COQ_MAKE_ALL_TARGET) && \
	   $(COQ_MAKE) install); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done && \
	$(TOUCH) .done_build_external_coq_tool

#
# Internal tools
#
build_internal_tools: .done_build_internal_tools

.done_build_internal_tools: \
  .done_build_external_tools \
  .done_build_zenon .done_build_zvtov
	@$(TOUCH) .done_build_internal_tools

# Zenon
$(ZENON_EXES): .done_build_zenon

.done_build_zenon: .done_build_external_tools
	@for i in $(ABSOLUTE_ZENON_SRC_DIR); do \
	  echo "--> Building $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $(ZENON_MAKE_ALL_tARGET) && \
	   $(MAKE) install); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done && \
	$(TOUCH) .done_build_zenon

# Zvtov
$(ZVTOV_EXES): .done_build_zvtov

.done_build_zvtov : .done_build_zenon
	@for i in $(ABSOLUTE_ZVTOV_SRC_DIR); do \
	  echo "--> Building $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $(ZVTOV_MAKE_ALL_TARGET) && \
           $(MAKE) install); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done && \
	$(TOUCH) .done_build_zvtov

#
# Deliverables
#

build_deliverables: .done_build_deliverables
.done_build_deliverables: .done_build_focalizec .done_build_focalizedep
	@$(TOUCH) .done_build_deliverables

# The focalize compiler
$(FOCALIZEC_EXES): .done_build_focalizec
.done_build_focalizec: .done_build_internal_tools
	@for i in $(ABSOLUTE_FOCALIZEC_SRC_DIR); do \
	  echo "--> Building $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $(FOCALIZEC_MAKE_ALL_TARGET)); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done && \
	$(TOUCH) .done_build_focalizec

# The focalize dependencies finder
$(FOCALIZEDEP_EXES): .done_build_focalizedep

.done_build_focalizedep: .done_build_focalizec
	@for i in $(ABSOLUTE_FOCALIZEDEP_SRC_DIR); do \
	  echo "--> Building $$i ..." >&2 && \
	  ($(CD) $$i && \
	   $(MAKE) $(FOCALIZEDEP_MAKE_ALL_TARGET)); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done && \
	$(TOUCH) .done_build_focalizedep

# To get a correct make all for internal tools we need to have a
# make_and_install target in each internal tool Makefile.
# This target should make all and then install the executable,
# if and only if, either:
# - there is no installed executable yet, or
# - ./executable is newer than the already installed one.
# This may be obtained via a .done_installation hidden file, with explicit
# dependancy ? Or may be a .to_do_installation and .done_installation ?
# or with a shell script that tests the above condition ?
# Another prerequisite is that the ``all'' target indeed does nothing if
# nothing has to be done (not even rebuild the same executable or copy it to
# a selected place).

clean:: clean_internal_tools clean_deliverables

install::
	@if [ ! -f .done_build_internal_tools ]; then \
	  echo 'you must run "make" before running "make install"' && \
	  exit 2; \
	fi && \
	if [ ! -d doc ]; then \
	  echo 'you must run "make docdir" before running "make install"' && \
	  exit 2; \
	fi && \
	for i in $(ABSOLUTE_FOCALIZEC_SRC_DIR); do \
	  echo "--> Installing $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $@); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done && \
	$(TOUCH) .done_install_focalizec && \
	$(TOUCH) .done_install_focalizedep

uninstall doc odoc depend::
	@for i in $(TOOLS_INTERNAL_DIRS); do \
	  echo "--> $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) $@); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

.PHONY: distclean_doc_dir
distclean_doc_dir:
	$(RM) $(DOCDIR_DIR)

docdir::
	$(MKDIR) $(DOCDIR_DIR) && \
	for i in $(TOOLS_INTERNAL_DIRS) $(DELIVERABLES_DIRS); do \
	  echo "--> $$i ..." >&2 && \
	  $(MKDIR) $(DOCDIR_DIR)/$$i && \
	  ($(CD) $$i && $(MAKE) $@) && \
	  if test -d $$i/$(DOC_ROOT_DIR); then \
	    $(CPR) $$i/$(DOC_ROOT_DIR)/* $(DOCDIR_DIR)/$$i/; \
	  fi; \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

#.PHONY: clean_internal_tools
clean_internal_tools: clean_zvtov clean_zenon

.PHONY: clean_zvtov
clean_zvtov:
	@$(RM) .done_build_internal_tools && \
	$(RM) .done_build_zvtov && \
	for i in $(ZVTOV_NAME); do \
	  $(RM) .done_build_$$i && \
	  $(TOUCH) $$i/.config_var; \
	done && \
	for i in $(ZVTOV_NAME); do \
	  echo "--> Cleaning $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) clean); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

.PHONY: clean_zenon
clean_zenon:
	@$(RM) .done_build_internal_tools && \
	$(RM) .done_build_zenon && \
	for i in $(ZENON_NAME); do \
	  $(RM) .done_build_$$i && \
	  $(TOUCH) $$i/.config_var; \
	done && \
	for i in $(ZENON_NAME); do \
	  echo "--> Cleaning $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) clean); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

.PHONY: distclean_internal_tools
distclean_internal_tools:
	@$(RM) .done_build_internal_tools && \
	for i in $(TOOLS_INTERNAL); do \
	  $(RM) .done_build_$$i; \
	done && \
	for i in $(TOOLS_INTERNAL_DIRS); do \
	  echo "--> Dist Cleaning $$i ..." >&2 && \
	  ($(CD) $$i && touch .config_var && $(MAKE) distclean); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

#.PHONY: clean_deliverables
clean_deliverables: clean_focalizedep clean_focalizec

.PHONY: clean_focalizec
clean_focalizec:
	@$(RM) .done_build_deliverables \
	$(RM) .done_build_focalizec && \
	for i in $(FOCALIZEC_NAME); do \
	  $(RM) .done_build_$$i && \
	  $(TOUCH) $$i/.config_var; \
	done && \
	for i in $(FOCALIZEC_NAME); do \
	  echo "--> Cleaning $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) clean); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

.PHONY: clean_focalizedep
clean_focalizedep:
	@$(RM) .done_build_deliverables \
	$(RM) .done_build_focalizedep && \
	for i in $(FOCALIZEDEP_NAME); do \
	  echo "--> Cleaning $$i ..." >&2 && \
	  ($(CD) $(ABSOLUTE_FOCALIZEC_SRC_DIR)/src/$$i && $(MAKE) clean); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done


.PHONY: distclean_deliverables
distclean_deliverables:
	@$(RM) .done_build_deliverables && \
	for i in $(DELIVERABLES); do \
	  $(RM) .done_build_$$i; \
	done && \
	for i in $(REV_DELIVERABLES_DIRS); do \
	  echo "--> Dist Cleaning $$i ..." >&2 && \
	  ($(CD) $$i && touch .config_var && $(MAKE) distclean); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

#.PHONY: clean_external_tools
clean_caml: clean_camlp5 clean_coq
	@$(RM) .done_build_external_caml_tool && \
	echo "--> Cleaning Caml ..." >&2 && \
	($(CD) $(ABSOLUTE_CAML_SRC_DIR) && $(MAKE) clean); \
	err=$$?; \
	echo "<-- Caml [$$err]" >&2 && \
	case $$err in 0);; *) exit $$err;; esac;

clean_camlp5: clean_coq
	@$(RM) .done_build_external_camlp5_tool && \
	echo "--> Cleaning Camlp5 ..." >&2 && \
	($(CD) $(ABSOLUTE_CAMLP5_SRC_DIR) && $(MAKE) clean); \
	err=$$?; \
	echo "<-- Camlp5 [$$err]" >&2 && \
	case $$err in 0);; *) exit $$err;; esac;

clean_coq: clean_internal_tools
	@$(RM) .done_build_external_coq_tool && \
	echo "--> Cleaning Coq ..." >&2 && \
	($(CD) $(ABSOLUTE_COQ_SRC_DIR) && $(MAKE) clean); \
	err=$$?; \
	echo "<-- Coq [$$err]" >&2 && \
	case $$err in 0);; *) exit $$err;; esac;

clean_external_tools: clean_caml clean_camlp5 clean_coq
	@$(RM) .done_build_external_tools

.PHONY: distclean_external_tools
distclean_external_tools: clean_install_external_tools_sources
	@$(RM) .done_build_external_coq_tool && \
	$(RM) .done_build_external_camlp5_tool && \
	$(RM) .done_build_external_caml_tool && \
	$(RM) .done_build_external_tools && \
	for i in $(TOOLS_EXTERNAL); do \
	  $(RM) .done_build_external_$$i_tool; \
	done && \
	for i in $(TAR_BALLS_DIR); do \
	  echo "--> Dist cleaning $$i ..." >&2 && \
	  ($(CD) $$i && $(MAKE) distclean); \
	  err=$$?; \
	  echo "<-- $$i [$$err]" >&2 && \
	  case $$err in 0);; *) exit $$err;; esac; \
	done

.PHONY: distclean_distribution
distclean_distribution:
	@if test -r Makefile.distribution; then \
	  $(MAKE) -f Makefile.distribution distclean; \
	fi

#.PHONY: distclean
distclean:: \
  distclean_external_tools distclean_internal_tools distclean_deliverables \
  distclean_distribution distclean_doc_dir unconfigure
