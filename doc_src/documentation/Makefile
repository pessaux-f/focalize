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

# $Id: Makefile,v 1.6 2011-05-27 07:33:41 weis Exp $

ROOT_DIR=../..

THIS_DIR=documentation

include $(ROOT_DIR)/Makefile.config

PROJECT_WEB_TARGET_DIR=$(PROJECT_WEB_SERVER):$(PROJECT_WEB_SITE_DIR)

DOC_SRC_DIR=$(ROOT_DIR)/doc/focalizec

# The reference manual compiled files.
REFERENCE_MANUAL_SRC_DIR=$(DOC_SRC_DIR)/tex/refman
REFERENCE_MANUAL_NAME=$(PROJECT_NAME_FULL)_refman
REFERENCE_MANUAL_OBJS=\
 $(REFERENCE_MANUAL_NAME).dvi\
 $(REFERENCE_MANUAL_NAME).ps\
 $(REFERENCE_MANUAL_NAME).pdf\
 $(REFERENCE_MANUAL_NAME).html

# The tutorial compiled files.
TUTORIAL_SRC_DIR=$(DOC_SRC_DIR)/tex/tutorial
TUTORIAL_NAME=$(PROJECT_NAME_FULL)_tutorial
TUTORIAL_OBJS=\
 $(TUTORIAL_NAME).dvi\
 $(TUTORIAL_NAME).ps\
 $(TUTORIAL_NAME).pdf\
 $(TUTORIAL_NAME).html

# The Unix manual pages.
MAN_PAGES_OBJS=\
 focalizec.$(PROJECT_MAN_EXT)\
 focalizedep.$(PROJECT_MAN_EXT)

HTMLC_INCLUDES =\
 -I $(ROOT_DIR)/doc_src/Includes\
 -env $(ROOT_DIR)/.config_var_for_shell -env project.env

SOURCES=index.shtml
BYT_OBJS=$(SOURCES:.shtml=.html)

# We cannot use the default behaviour for BYT_OBJS in Makefile.common,
# since the default rules for .dvi -> .ps, dvi -> pdf, etc.
# would conflict with the simple copies we do here, unless we redefine properly the
# compilers associated to the rules (DVIPS, DVIPDF, and so on).
include $(ROOT_DIR)/Makefile.common

DOC_OBJS=$(BYT_OBJS) $(MAN_PAGES_OBJS) $(REFERENCE_MANUAL_OBJS)\
 $(TUTORIAL_OBJS)

all: .done_copy_documentation

clean-local:
	$(RM) .done_copy_man_pages
	$(RM) .done_copy_documentation
	$(RM) $(DOC_OBJS)

.done_copy_man_pages: $(DOC_SRC_DIR)/man/focalizec.$(PROJECT_MAN_EXT)\
 $(DOC_SRC_DIR)/man/focalizedep.$(PROJECT_MAN_EXT)
	$(CP) $(DOC_SRC_DIR)/man/focalizec.$(PROJECT_MAN_EXT) focalizec.$(PROJECT_MAN_EXT) &&\
	$(CP) $(DOC_SRC_DIR)/man/focalizedep.$(PROJECT_MAN_EXT) focalizedep.$(PROJECT_MAN_EXT)
	$(TOUCH) .done_copy_man_pages

# Generate the copies of the reference manual and the tutorial to put them
# on the Web site afterwards.
.done_copy_documentation: .done_copy_man_pages $(REFERENCE_MANUAL_SRC_DIR)/refman.dvi\
 $(TUTORIAL_SRC_DIR)/tutorial.dvi
	$(CP) $(REFERENCE_MANUAL_SRC_DIR)/refman.dvi $(REFERENCE_MANUAL_NAME).dvi &&\
	$(CP) $(REFERENCE_MANUAL_SRC_DIR)/refman.ps $(REFERENCE_MANUAL_NAME).ps &&\
	$(CP) $(REFERENCE_MANUAL_SRC_DIR)/refman.pdf $(REFERENCE_MANUAL_NAME).pdf &&\
	$(CP) $(REFERENCE_MANUAL_SRC_DIR)/refman.html $(REFERENCE_MANUAL_NAME).html &&\
	$(CP) $(TUTORIAL_SRC_DIR)/tutorial.dvi $(TUTORIAL_NAME).dvi &&\
	$(CP) $(TUTORIAL_SRC_DIR)/tutorial.ps $(TUTORIAL_NAME).ps &&\
	$(CP) $(TUTORIAL_SRC_DIR)/tutorial.pdf $(TUTORIAL_NAME).pdf &&\
	$(CP) $(TUTORIAL_SRC_DIR)/tutorial.html $(TUTORIAL_NAME).html &&\
	$(TOUCH) .done_copy_documentation

install-local:
	$(CHMODGRW) $(DOC_OBJS) &&\
	$(SCP) $(DOC_OBJS) $(PROJECT_WEB_TARGET_DIR)/$(THIS_DIR)/


uninstall-local:
	$(SSH) $(PROJECT_WEB_SERVER)\
	  $(CD) $(PROJECT_WEB_SITE_DIR)/$(THIS_DIR)/\; $(RM) $(DOC_OBJS)

# We need the copies of the various formats of the refman in order
# to calculate their sizes. Hence this extra dependency.
index.html: .done_copy_documentation
