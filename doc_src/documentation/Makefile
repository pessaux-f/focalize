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

THIS_DIR = documentation

include $(ROOT_DIR)/Makefile.config

PROJECT_WEB_TARGET_DIR=$(PROJECT_WEB_SERVER):$(PROJECT_WEB_SITE_DIR)

DOC_SRC_DIR=$(ROOT_DIR)/doc/focalizec

# The reference manual compiled files.
REFERENCE_MANUAL_SRC_DIR=$(DOC_SRC_DIR)/tex/refman
REFERENCE_MANUAL_NAME=$(PROJECT_NAME_FULL)_refman
REFERENCE_MANUAL_OBJS=\
 $(REFERENCE_MANUAL_NAME).ps\
 $(REFERENCE_MANUAL_NAME).pdf\
 $(REFERENCE_MANUAL_NAME).dvi\
 $(REFERENCE_MANUAL_NAME).html

# The Unix manual pages.
MAN_PAGES_OBJS=\
 focalizec.$(PROJECT_MAN_EXT) \
 focalizedep.$(PROJECT_MAN_EXT)

SOURCES=index.shtml
BYT_OBJS=$(SOURCES:.shtml=.html)

ALL_OBJS=$(BYT_OBJS) $(MAN_PAGES_OBJS) $(REFERENCE_MANUAL_OBJS)

include $(ROOT_DIR)/Makefile.common

HTMLC_INCLUDES =\
 -I $(ROOT_DIR)/doc_src/Includes\
 -env $(ROOT_DIR)/.config_var_for_shell -env project.env

all:: .done_copy_documentation $(BYT_OBJS)

clean::
	$(RM) .done_copy_man_pages
.done_copy_man_pages: $(DOC_SRC_DIR)/man/focalizec.$(PROJECT_MAN_EXT)\
 $(DOC_SRC_DIR)/man/focalizedep.$(PROJECT_MAN_EXT)
	$(CP) $(DOC_SRC_DIR)/man/focalizec.$(PROJECT_MAN_EXT) focalizec.$(PROJECT_MAN_EXT) && \
	$(CP) $(DOC_SRC_DIR)/man/focalizedep.$(PROJECT_MAN_EXT) focalizedep.$(PROJECT_MAN_EXT)
	$(TOUCH) .done_copy_man_pages

clean::
	$(RM) .done_copy_documentation
.done_copy_documentation: .done_copy_man_pages\
  $(REFERENCE_MANUAL_SRC_DIR)/refman.ps\
  $(REFERENCE_MANUAL_SRC_DIR)/refman.dvi\
  $(REFERENCE_MANUAL_SRC_DIR)/refman.pdf\
  $(REFERENCE_MANUAL_SRC_DIR)/refman.html

.done_copy_documentation:
	$(CP) $(REFERENCE_MANUAL_SRC_DIR)/refman.ps $(REFERENCE_MANUAL_NAME).ps && \
	$(CP) $(REFERENCE_MANUAL_SRC_DIR)/refman.dvi $(REFERENCE_MANUAL_NAME).dvi && \
	$(CP) $(REFERENCE_MANUAL_SRC_DIR)/refman.pdf $(REFERENCE_MANUAL_NAME).pdf && \
	$(CP) $(REFERENCE_MANUAL_SRC_DIR)/refman.html $(REFERENCE_MANUAL_NAME).html && \
	$(TOUCH) .done_copy_documentation

clean::
	$(RM) $(ALL_OBJS)

install::
	$(CHMODGRW) $(ALL_OBJS) && \
	  $(SCP) $(ALL_OBJS) $(PROJECT_WEB_TARGET_DIR)/$(THIS_DIR)/ && \
	$(RM) $(BYT_OBJS)

index.html: .done_copy_documentation
