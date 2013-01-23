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
#  Copyright 2007 - 2012 LIP6 and INRIA                                #
#            2012 ENSTA ParisTech                                      #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

ROOT_DIR=..

SUB_DIRS=contacts documentation download resources

include $(ROOT_DIR)/Makefile.common

HTMLC_INCLUDES=-I $(ROOT_DIR)/doc_src/Includes -env project.env

SOURCES=index.shtml
BYT_OBJS=$(SOURCES:.shtml=.html)

# Install command: scp xxx pessaux@modulogic.inria.fr:/home/www/focalize/
PROJECT_WEB_SITE_DIR=/home/www/focalize
PROJECT_WEB_TARGET_DIR="pessaux@modulogic.inria.fr":$(PROJECT_WEB_SITE_DIR)

all: $(BYT_OBJS)
	@for i in $(SUB_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i &&\
	   $(MAKE) all);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" >&2 &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done

clean:
	$(RM) $(BYT_OBJS)
	@for i in $(SUB_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i &&\
	   $(MAKE) clean);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" >&2 &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done

# Installation, deinstallation.

# We must operate on the HTTP server: we use scp to copy a file remotely,
# and ssh + rm to remove a file remotely.

# We chmod g+rw the files that we transfer to the HTTP server in order to grant
# other developers the right to install them afterwards.
install:
	$(CHMODGRW) css/project.css &&\
	  $(SCP) css/project.css $(PROJECT_WEB_TARGET_DIR)/css &&\
	$(CHMODGRW) imgs/*.* &&\
	  $(SCP) imgs/*.* $(PROJECT_WEB_TARGET_DIR)/imgs &&\
	$(CHMODGRW) $(BYT_OBJS) &&\
	  $(SCP) $(BYT_OBJS) $(PROJECT_WEB_TARGET_DIR)
	@for i in $(SUB_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i &&\
	   $(MAKE) PROJECT_WEB_TARGET_DIR='$(PROJECT_WEB_TARGET_DIR)' install);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" >&2 &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done

uninstall:
	$(SSH) $(PROJECT_WEB_SERVER)\
	  $(CD) $(PROJECT_WEB_SITE_DIR)/css; $(RM) project.css
	$(SSH) $(PROJECT_WEB_SERVER)\
	  $(CD) $(PROJECT_WEB_SITE_DIR)/imgs; $(RM) *.*
	$(SSH) $(PROJECT_WEB_SERVER)\
	  $(CD) $(PROJECT_WEB_SITE_DIR); $(RM) $(BYT_OBJS)
	@for i in $(SUB_DIRS); do\
	  echo "--> $$i ..." >&2 &&\
	  ($(CD) $$i &&\
	   $(MAKE) PROJECT_WEB_TARGET_DIR='$(PROJECT_WEB_TARGET_DIR)' uninstall);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" >&2 &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done
