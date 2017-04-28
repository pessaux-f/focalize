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

ROOT_DIR=.

SUB_DIRS= zvtov focalizec

# No need to include Makefile.rules and .config_var (this latter not
# existing from a scratch checkout). So no need to include the whole
# stuff via Makefile.common. Just Makefile.utils is sufficient.
include $(ROOT_DIR)/Makefile.utils

CONFIGURE_FLAGS=

# Default rule, building everything with default settings.
all:
	@echo "Building Zvtov..."
	@($(CD) zvtov &&\
		$(MAKE) depend &&\
		$(MAKE) all &&\
		$(MAKE) doc &&\
	  echo "Installing..." &&\
		$(MAKE) install);\
	  err=$$?;\
	  case $$err in 0);; *) exit $$err;; esac;
	@echo "Building FoCaLiZeC and stuff..."
	@($(CD) focalizec &&\
		$(MAKE) depend &&\
		$(MAKE) world &&\
		$(MAKE) doc &&\
	  echo "Installing..." &&\
		$(MAKE) install);\
	  err=$$?;\
	  case $$err in 0);; *) exit $$err;; esac;


# Target for building FoCaLiZe using Opam.
#
# I cannot figure out how to assign CONFIGURE_FLAGS to the two words
# "--install_prefix" and "%{bin}%" inside an opam file.
#
# The 'yes' program is used to skip all the questions interactively
# asked by the configure script to omit optional dependencies.
#
# Also, do not build the doc since we do not know if LaTeX and Hevea
# are present

opam_build:
	@echo "Toplevel configuration..."
	@yes '' | ./configure --install_prefix $(OPAM_PREFIX)
	@echo "Building Zvtov..."
	@($(CD) zvtov &&\
		$(MAKE) depend &&\
		$(MAKE) all &&\
	  echo "Installing..." &&\
		$(MAKE) install);\
	  err=$$?;\
	  case $$err in 0);; *) exit $$err;; esac;
	@echo "Building FoCaLiZeC and stuff..."
	@($(CD) focalizec &&\
		$(MAKE) depend &&\
		$(MAKE) world &&\
	  echo "Installing..." &&\
		$(MAKE) install);\
	  err=$$?;\
	  case $$err in 0);; *) exit $$err;; esac;


opam_install:
	@echo "Toplevel configuration..."
	@yes '' | ./configure --install_prefix $(OPAM_PREFIX)
	@echo "Installing..."
	$(MAKE) install


opam_remove:
	@echo "Toplevel configuration..."
	@yes '' | ./configure --install_prefix $(OPAM_PREFIX)
	@echo "Uninstalling..."
	$(MAKE) uninstall

# Alternative rule, building everything with user-specified settings.
interactive-all:
	make CONFIGURE_FLAGS=-interactive all


clean:
	@for i in $(SUB_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i && $(MAKE) $@);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" >&2 &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done


install:
	@for i in $(SUB_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i && $(MAKE) $@);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" >&2 &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done


uninstall:
	@for i in $(SUB_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i && $(MAKE) $@);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" >&2 &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done


distrib:
	$(RM) -f focalize-0.9.2.tgz
	($(CD) .. &&\
	tar cvzf focalize/focalize-0.9.2.tgz -T focalize/files-for-distrib.lst)
