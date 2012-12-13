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

# $Id: Makefile,v 1.7 2012-02-24 15:58:07 pessaux Exp $

ROOT_DIR=../..

THIS_DIR=documentation

DOC_SRC_DIR=$(ROOT_DIR)/focalizec/doc_src

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
MAN_PAGES_OBJS= focalizec.1 focalizedep.1

# We cannot use the default behaviour for BYT_OBJS in Makefile.common,
# since the default rules for .dvi -> .ps, dvi -> pdf, etc.
# would conflict with the simple copies we do here, unless we redefine properly the
# compilers associated to the rules (DVIPS, DVIPDF, and so on).
include $(ROOT_DIR)/Makefile.common

HTMLC_INCLUDES =\
 -I $(ROOT_DIR)/doc_src/Includes\
 -env $(ROOT_DIR)/.config_var_for_shell -env project.env

SOURCES=index.shtml
BYT_OBJS=$(SOURCES:.shtml=.html)

DOC_OBJS=$(BYT_OBJS) $(MAN_PAGES_OBJS) $(REFERENCE_MANUAL_OBJS)\
 $(TUTORIAL_OBJS)

all: .done_copy_documentation .done_copy_man_pages

clean:
	$(RM) .done_copy_man_pages
	$(RM) .done_copy_documentation
	$(RM) $(DOC_OBJS)

.done_copy_man_pages: $(DOC_SRC_DIR)/man/focalizec.1\
		 $(DOC_SRC_DIR)/man/focalizedep.1
	$(CP) $(DOC_SRC_DIR)/man/focalizec.1 . &&\
	$(CP) $(DOC_SRC_DIR)/man/focalizedep.1 .
	$(TOUCH) .done_copy_man_pages

# Generate the copies of the reference manual and the tutorial to put them
# on the Web site afterwards.
.done_copy_documentation:  $(REFERENCE_MANUAL_SRC_DIR)/refman.pdf\
 $(TUTORIAL_SRC_DIR)/tutorial.pdf
	$(CP) $(REFERENCE_MANUAL_SRC_DIR)/refman.dvi . &&\
	$(CP) $(REFERENCE_MANUAL_SRC_DIR)/refman.ps . &&\
	$(CP) $(REFERENCE_MANUAL_SRC_DIR)/refman.pdf . &&\
	$(CP) $(REFERENCE_MANUAL_SRC_DIR)/refman.html . &&\
	$(CP) $(TUTORIAL_SRC_DIR)/tutorial.dvi . &&\
	$(CP) $(TUTORIAL_SRC_DIR)/tutorial.ps . &&\
	$(CP) $(TUTORIAL_SRC_DIR)/tutorial.pdf . &&\
	$(CP) $(TUTORIAL_SRC_DIR)/tutorial.html . &&\
	$(TOUCH) .done_copy_documentation

install:
	$(CHMODGRW) $(DOC_OBJS) &&\
	$(SCP) $(DOC_OBJS) $(PROJECT_WEB_TARGET_DIR)/$(THIS_DIR)/

uninstall:
	$(SSH) $(PROJECT_WEB_SERVER)\
	  $(CD) $(PROJECT_WEB_SITE_DIR)/$(THIS_DIR)/\; $(RM) $(DOC_OBJS)

# We need the copies of the various formats of the refman in order
# to calculate their sizes. Hence this extra dependency.
index.html: .done_copy_documentation
