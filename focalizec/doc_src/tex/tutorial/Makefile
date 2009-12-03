#**********************************************************************#
#                                                                      #
#                        FoCaLiZe compiler                             #
#                                                                      #
#            François Pessaux                                          #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.7 2009-12-03 17:01:13 habib Exp $

ROOT_DIR = ../../..

include $(ROOT_DIR)/Makefile.config

TEX_FILES = tutorial.tex

HVA_FILES = tutorial.hva

FCL_FILES = superset.fcl subset.fcl mylist.fcl main.fcl

FCL_OBJS = $(FCL_FILES:.fcl=.cmo)

DOC_OBJS = tutorial.dvi tutorial.ps tutorial.pdf tutorial.html

TEXINPUT = /usr/share/texmf/tex/latex//:$(TEXINPUTS)
LATEX = TEXINPUTS=$(TEXINPUT); latex
HEVEA = TEXINPUTS=$(TEXINPUT); hevea
IMAGEN = imagen -extra "ppmquant 255"

DVIPS = dvips
DVIPDF = dvipdf

# DOCDIR_DIR is the directory doc that this Makefile must populate.
DOCDIR_DIR=$(DOC_ROOT_DIR)/tex/tutorial

# DOC_TGT_DIR is the target directory for the installation of the documentation
DOC_TGT_DIR:=$(DOC_DIR)/tex/$(PROJECT_NAME_FULL)/tutorial

include $(ROOT_DIR)/Makefile.common

FOCALIZE_COMPILER = $(ROOT_DIR)/src/toplevel/focalizec

all doc docdir:: byt $(DOC_OBJS)

byt:: $(FCL_OBJS)

docdir:: $(DOC_OBJS)
	$(MKDIR) $(DOCDIR_DIR) && \
	$(CP) $(DOC_OBJS) $(DOCDIR_DIR)/

tutorial.dvi: $(TEX_FILES)
	$(LATEX) tutorial.tex && \
	$(LATEX) tutorial.tex && \
	$(LATEX) tutorial.tex

tutorial.html: tutorial.dvi ${HVA_FILES}
	$(HEVEA) -fix ${HVA_FILES} tutorial.tex

clean::
	$(RM) *.aux *.log *.dvi *.idx *.ilg *.ind *.toc
	$(RM) *.haux *.htoc
	$(RM) tutorial*.gif tutorial.image.tex
	$(RM) $(DOC_OBJS)

distclean::
	$(RM) $(DOCDIR_DIR)

install:
	$(MKDIR) $(DOC_TGT_DIR) && \
	$(CP) $(DOC_OBJS) $(DOC_TGT_DIR)

uninstall:
	$(RM) $(DOC_TGT_DIR)
