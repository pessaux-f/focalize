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
#  Copyright 2009 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.2 2009-03-02 10:19:30 weis Exp $

ROOT_DIR = ../..

THIS_DIR = contacts

include $(ROOT_DIR)/Makefile.config

HTMLC_INCLUDES = -I $(ROOT_DIR)/doc_src/Includes \
  -env $(ROOT_DIR)/.config_var_for_shell -env project.env

SOURCES=index.shtml
BYT_OBJS=$(SOURCES:.shtml=.html)

include $(ROOT_DIR)/Makefile.common

PROJECT_WEB_TARGET_DIR=$(PROJECT_WEB_SERVER):$(PROJECT_WEB_SITE_DIR)

clean::
	$(RM) $(BYT_OBJS)

install::
	$(CHMODGRW) $(BYT_OBJS)
	$(SCP) $(BYT_OBJS) $(PROJECT_WEB_TARGET_DIR)/$(THIS_DIR)/
	$(RM) $(BYT_OBJS)
