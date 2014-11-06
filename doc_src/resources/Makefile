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
#  Copyright 2007 -2012 LIP6 and INRIA                                 #
#            2012 ENSTA ParisTech                                      #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# To manually install:
# scp *.html pessaux@sosie.inria.fr:/home/www/focalize/resources/

ROOT_DIR=../..

THIS_DIR=resources

include $(ROOT_DIR)/Makefile.common

HTMLC_INCLUDES=-I $(ROOT_DIR)/doc_src/Includes -env project.env

SOURCES=index.shtml
BYT_OBJS=$(SOURCES:.shtml=.html)

all: $(BYT_OBJS)

clean:
	$(RM) $(BYT_OBJS)

install:
	$(CHMODGRW) $(BYT_OBJS)
	$(SCP) $(BYT_OBJS) $(PROJECT_WEB_TARGET_DIR)/$(THIS_DIR)/
	$(RM) $(BYT_OBJS)

uninstall:
	$(SSH) $(PROJECT_WEB_SERVER)\
	  $(CD) $(PROJECT_WEB_SITE_DIR)/$(THIS_DIR)/\; $(RM) $(BYT_OBJS)
