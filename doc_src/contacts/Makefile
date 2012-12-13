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

ROOT_DIR=../..

THIS_DIR=contacts

include $(ROOT_DIR)/Makefile.common

HTMLC_INCLUDES=-I $(ROOT_DIR)/doc_src/Includes -env project.env

SOURCES=index.shtml
BYT_OBJS=$(SOURCES:.shtml=.html)

all: $(BYT_OBJS)

clean:
	$(RM) $(BYT_OBJS)

install:
	$(CHMODGRW) $(BYT_OBJS) &&\
	$(SCP) $(BYT_OBJS) $(PROJECT_WEB_TARGET_DIR)/$(THIS_DIR)/

uninstall:
	$(SSH) $(PROJECT_WEB_SERVER)\
	  $(CD) $(PROJECT_WEB_SITE_DIR)/$(THIS_DIR)/\; $(RM) $(BYT_OBJS)
