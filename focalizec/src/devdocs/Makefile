#**********************************************************************#
#                                                                      #
#                        FoCaLize compiler                             #
#                                                                      #
#            Fran�ois Pessaux                                          #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.4 2012-10-15 15:40:06 pessaux Exp $

ROOT_DIR=../../..

TEX_FILES=\
 codegen_model.tex macros.tex compiler_src_architecture.tex\
 legacy.tex phd_changes.tex environments.tex lexing_parsing.tex\
 scoping.tex coqrecfuns.tex mathpartir.sty

DOC_OBJS=legacy.dvi legacy.ps legacy.pdf

TEXINPUT=/usr/share/texmf/tex/latex//:$(TEXINPUTS)
LATEX=TEXINPUTS=$(TEXINPUT); latex

include $(ROOT_DIR)/Makefile.common


all: $(DOC_OBJS)

legacy.dvi: $(TEX_FILES)
	$(LATEX) legacy.tex
	makeindex legacy
	$(LATEX) legacy.tex && latex legacy.tex

legacy.pdf: legacy.dvi
	dvipdf legacy.dvi

legacy.ps: legacy.dvi
	dvips legacy.dvi

clean:
	$(RM) *.aux *.log *.dvi *.idx *.ilg *.ind *.toc
	$(RM) $(DOC_OBJS)
