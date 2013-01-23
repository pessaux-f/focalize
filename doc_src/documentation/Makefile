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

THIS_DIR=documentation

DOC_SRC_DIR=$(ROOT_DIR)/focalizec/doc_src

# The reference manual compiled files.
REFERENCE_MANUAL_SRC_DIR=$(DOC_SRC_DIR)/tex/refman
REFERENCE_MANUAL_NAME=refman
REFERENCE_MANUAL_OBJS=\
 $(REFERENCE_MANUAL_NAME).dvi\
 $(REFERENCE_MANUAL_NAME).ps\
 $(REFERENCE_MANUAL_NAME).pdf\
 $(REFERENCE_MANUAL_NAME).html

# The tutorial compiled files.
TUTORIAL_SRC_DIR=$(DOC_SRC_DIR)/tex/tutorial
TUTORIAL_NAME=tutorial
TUTORIAL_OBJS=\
 $(TUTORIAL_NAME).dvi\
 $(TUTORIAL_NAME).ps\
 $(TUTORIAL_NAME).pdf\
 $(TUTORIAL_NAME).html

# The proofs tutorial compiled files.
PROOFS_TUTORIAL_SRC_DIR=$(DOC_SRC_DIR)/tex/tuto_proofs
PROOFS_TUTORIAL_NAME=tuto_proofs
PROOFS_TUTORIAL_OBJS=\
 $(PROOFS_TUTORIAL_NAME).dvi\
 $(PROOFS_TUTORIAL_NAME).ps\
 $(PROOFS_TUTORIAL_NAME).pdf\
 $(PROOFS_TUTORIAL_NAME).html

# The Unix manual pages.
MAN_PAGES_OBJS= focalizec.1 focalizedep.1

# We cannot use the default behaviour for BYT_OBJS in Makefile.common,
# since the default rules for .dvi -> .ps, dvi -> pdf, etc.
# would conflict with the simple copies we do here, unless we redefine properly the
# compilers associated to the rules (DVIPS, DVIPDF, and so on).
include $(ROOT_DIR)/Makefile.common

HTMLC_INCLUDES = -I $(ROOT_DIR)/doc_src/Includes -env project.env

# Install command: scp xxx pessaux@modulogic.inria.fr:/home/www/focalize/documentation/

SOURCES=index.shtml
BYT_OBJS=$(SOURCES:.shtml=.html)

DOC_OBJS=$(BYT_OBJS) $(MAN_PAGES_OBJS) $(REFERENCE_MANUAL_OBJS)\
 $(TUTORIAL_OBJS) $(PROOFS_TUTORIAL_OBJS)

all: .done_copy_documentation .done_copy_man_pages $(BYT_OBJS)

clean:
	$(RM) .done_copy_man_pages
	$(RM) .done_copy_documentation
	$(RM) $(DOC_OBJS) $(BYT_OBJS)

.done_copy_man_pages: $(DOC_SRC_DIR)/man/focalizec.1\
		 $(DOC_SRC_DIR)/man/focalizedep.1
	$(CP) $(DOC_SRC_DIR)/man/focalizec.1 . &&\
	$(CP) $(DOC_SRC_DIR)/man/focalizedep.1 .
	$(TOUCH) .done_copy_man_pages

# Generate the copies of the reference manual and the tutorial to put them
# on the Web site afterwards.
.done_copy_documentation:  $(REFERENCE_MANUAL_SRC_DIR)/refman.pdf\
 $(TUTORIAL_SRC_DIR)/tutorial.pdf $(PROOFS_TUTORIAL_SRC_DIR)/tuto_proofs.pdf
	$(CP) $(REFERENCE_MANUAL_SRC_DIR)/refman.dvi . &&\
	$(CP) $(REFERENCE_MANUAL_SRC_DIR)/refman.ps . &&\
	$(CP) $(REFERENCE_MANUAL_SRC_DIR)/refman.pdf . &&\
	$(CP) $(REFERENCE_MANUAL_SRC_DIR)/refman.html . &&\
	$(CP) $(TUTORIAL_SRC_DIR)/tutorial.dvi . &&\
	$(CP) $(TUTORIAL_SRC_DIR)/tutorial.ps . &&\
	$(CP) $(TUTORIAL_SRC_DIR)/tutorial.pdf . &&\
	$(CP) $(TUTORIAL_SRC_DIR)/tutorial.html . &&\
	$(CP) $(PROOFS_TUTORIAL_SRC_DIR)/tuto_proofs.dvi . &&\
	$(CP) $(PROOFS_TUTORIAL_SRC_DIR)/tuto_proofs.ps . &&\
	$(CP) $(PROOFS_TUTORIAL_SRC_DIR)/tuto_proofs.pdf . &&\
	$(CP) $(PROOFS_TUTORIAL_SRC_DIR)/tuto_proofs.html . &&\
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
