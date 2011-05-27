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

# $Id: Makefile,v 1.4 2011-05-27 07:33:41 weis Exp $

ROOT_DIR=../..

THIS_DIR=contacts

include $(ROOT_DIR)/Makefile.config

HTMLC_INCLUDES=-I $(ROOT_DIR)/doc_src/Includes\
 -env $(ROOT_DIR)/.config_var_for_shell -env project.env

SOURCES=index.shtml
BYT_OBJS=$(SOURCES:.shtml=.html)

include $(ROOT_DIR)/Makefile.common

PROJECT_WEB_TARGET_DIR=$(PROJECT_WEB_SERVER):$(PROJECT_WEB_SITE_DIR)

clean-loca:
	$(RM) $(BYT_OBJS)

install-local:
	$(CHMODGRW) $(BYT_OBJS) &&\
	$(SCP) $(BYT_OBJS) $(PROJECT_WEB_TARGET_DIR)/$(THIS_DIR)/

uninstall-local:
	$(SSH) $(PROJECT_WEB_SERVER)\
	  $(CD) $(PROJECT_WEB_SITE_DIR)/$(THIS_DIR)/\; $(RM) $(BYT_OBJS)
