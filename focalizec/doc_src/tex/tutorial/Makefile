#**********************************************************************#
#                                                                      #
#                        FoCaLize compiler                             #
#                                                                      #
#            François Pessaux                                          #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.5 2009-10-01 07:06:20 weis Exp $

ROOT_DIR = ../../..

include $(ROOT_DIR)/Makefile.config

TEX_FILES = tutorial.tex

HVA_FILES = tutorial.hva

DOC_OBJS = tutorial.dvi tutorial.ps tutorial.pdf tutorial.html

TEXINPUT = /usr/share/texmf/tex/latex//:$(TEXINPUTS)
LATEX = TEXINPUTS=$(TEXINPUT); latex
HEVEA = TEXINPUTS=$(TEXINPUT); hevea
IMAGEN = imagen -extra "ppmquant 255"

DVIPS = dvips
DVIPDF = dvipdf

include $(ROOT_DIR)/Makefile.common

# DOCDIR_DIR is the directory doc that this Makefile must populate.
DOCDIR_DIR=$(DOC_ROOT_DIR)/tex/tutorial

# DOC_TGT_DIR is the target directory for the installation of the documentation
DOC_TGT_DIR:=$(DOC_DIR)/tex/$(PROJECT_NAME_FULL)/tutorial

all doc docdir:: $(DOC_OBJS)

docdir:: $(DOC_OBJS)
	$(MKDIR) $(DOCDIR_DIR) && \
	$(CP) $(DOC_OBJS) $(DOCDIR_DIR)/

tutorial.dvi: $(TEX_FILES)
	$(LATEX) tutorial.tex
	$(LATEX) tutorial.tex
	$(LATEX) tutorial.tex

#tutorial.pdf: tutorial.dvi
#	$(DVIPDF) tutorial.dvi

#tutorial.ps: tutorial.dvi
#	$(DVIPS) tutorial.dvi -o $@

tutorial.html: tutorial.dvi ${HVA_FILES}
	$(HEVEA) -fix ${HVA_FILES} tutorial.tex

clean::
	$(RM) *.aux *.log *.dvi *.idx *.ilg *.ind *.toc
	$(RM) *.haux *.htoc
	$(RM) tutorial*.gif tutorial.image.tex
	$(RM) $(DOC_OBJS)

distclean:: clean
	$(RM) $(DOCDIR_DIR)

install:
	$(MKDIR) $(DOC_TGT_DIR) && \
	$(CP) $(DOC_OBJS) $(DOC_TGT_DIR)

uninstall:
	$(RM) $(DOC_TGT_DIR)
