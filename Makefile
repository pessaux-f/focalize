#**********************************************************************#
#                                                                      #
#                        FoCaLiZe compiler                             #
#                                                                      #
#            Fran�ois Pessaux                                          #
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
	@echo "Toplevel configuration..."
	@./configure $(CONFIGURE_FLAGS)
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


uninstall:
	@for i in $(SUB_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i && $(MAKE) $@);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" >&2 &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done


distrib:
	$(RM) -f focalize-0.9.1.tgz
	($(CD) .. &&\
	tar cvzf focalize/focalize-0.9.1.tgz -T focalize/files-for-distrib.lst)
