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

# $Id: Makefile,v 1.9 2009-12-04 09:14:37 weis Exp $

ROOT_DIR = ../../..

include $(ROOT_DIR)/Makefile.config

TEX_FILES = tutorial.tex

HVA_FILES = tutorial.hva

FOCALIZE_SRC = superset.fcl subset.fcl mylist.fcl main.fcl

FOCALIZE_FOS = $(FOCALIZE_SRC:.fcl=.fo)
FOCALIZE_MLS = $(FOCALIZE_SRC:.fcl=.ml)

FOCALIZE_CMIS = $(FOCALIZE_SRC:.fcl=.cmi)

FOCALIZE_BYT_OBJS = $(FOCALIZE_MLS:.ml=.cmo)
FOCALIZE_BIN_OBJS = $(FOCALIZE_BYT_OBJS:.cmo=.cmx)

FOCALIZE_ZVS = $(FOCALIZE_SRC:.fcl=.zv)
FOCALIZE_VS = $(FOCALIZE_ZVS:.zv=.v)
FOCALIZE_VOS = $(FOCALIZE_VS:.v=.vo)

FOCALIZE_DOCS = $(FOCALIZE_SRC:.fcl=.fcd)

FOCALIZE_XML = $(FOCALIZE_DOCS:.fcd=.xml)

DOC_OBJS = tutorial.dvi tutorial.ps tutorial.pdf tutorial.html

# Defining defaults from Makefile.common
include $(ROOT_DIR)/Makefile.common
# Now overwriting defaults for this directory purposes.

# Already defined in Makefile.common
#DVIPS = dvips
#DVIPDF = dvipdf

TEXINPUT = /usr/share/texmf/tex/latex//:$(TEXINPUTS)
LATEX = TEXINPUTS=$(TEXINPUT); latex
HEVEA = TEXINPUTS=$(TEXINPUT); hevea
IMAGEN = imagen -extra "ppmquant 255"

# DOCDIR_DIR is the directory doc that this Makefile must populate.
DOCDIR_DIR=$(DOC_ROOT_DIR)/tex/tutorial

# DOC_TGT_DIR is the target directory for the installation of the documentation
DOC_TGT_DIR:=$(DOC_DIR)/tex/$(PROJECT_NAME_FULL)/tutorial

FOCALIZE_COMPILER = $(ROOT_DIR)/src/toplevel/focalizec
FOCALIZE_COMP_FLAGS = -I $(ROOT_DIR)/src/stdlib

all doc docdir:: byt $(DOC_OBJS)

byt:: $(FOCALIZE_BYT_OBJS)

clean::
	$(RM) $(FOCALIZE_MLS) $(FOCALIZE_ZVS) && \
	$(RM) $(FOCALIZE_VS)

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
