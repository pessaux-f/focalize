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
#  Copyright 2009 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.6 2012-10-15 15:49:42 pessaux Exp $

ROOT_DIR=..

include $(ROOT_DIR)/Makefile.config

SUB_DIRS=contacts documentation download resources

HTMLC_INCLUDES=-I $(ROOT_DIR)/doc_src/Includes\
 -env $(ROOT_DIR)/.config_var_for_shell -env project.env

SOURCES=index.shtml
BYT_OBJS=$(SOURCES:.shtml=.html)

include $(ROOT_DIR)/Makefile.common

# PROJECT_WEB_TARGET_DIR=$(PROJECT_WEB_SERVER):$(PROJECT_WEB_SITE_DIR)
PROJECT_WEB_TARGET_DIR="pessaux@focalize.inria.fr":$(PROJECT_WEB_SITE_DIR)

byt clean_local distclean_local install-local uninstall-local:
	@for i in $(SUB_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i &&\
	   $(MAKE) PROJECT_WEB_TARGET_DIR='$(PROJECT_WEB_TARGET_DIR)' $@);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" >&2 &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done

clean-local:
	$(RM) $(BYT_OBJS)

# Installation, deinstallation.

# We must operate on the HTTP server: we use scp to copy a file remotely,
# and ssh + rm to remove a file remotely.

# We chmod g+rw the files that we transfer to the HTTP server in order to grant
# other developers the right to install them afterwards.
install-local:
	$(CHMODGRW) css/project.css &&\
	  $(SCP) css/project.css $(PROJECT_WEB_TARGET_DIR)/css &&\
	$(CHMODGRW) imgs/*.* &&\
	  $(SCP) imgs/*.* $(PROJECT_WEB_TARGET_DIR)/imgs &&\
	$(CHMODGRW) $(BYT_OBJS) &&\
	  $(SCP) $(BYT_OBJS) $(PROJECT_WEB_TARGET_DIR)

uninstall-local:
	$(SSH) $(PROJECT_WEB_SERVER)\
	  $(CD) $(PROJECT_WEB_SITE_DIR)/css; $(RM) project.css
	$(SSH) $(PROJECT_WEB_SERVER)\
	  $(CD) $(PROJECT_WEB_SITE_DIR)/imgs; $(RM) *.*
	$(SSH) $(PROJECT_WEB_SERVER)\
	  $(CD) $(PROJECT_WEB_SITE_DIR); $(RM) $(BYT_OBJS)
