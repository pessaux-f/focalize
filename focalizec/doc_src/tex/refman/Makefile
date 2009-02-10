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

# $Id: Makefile,v 1.17 2009-02-10 09:59:45 weis Exp $

ROOT_DIR = ../../..

include $(ROOT_DIR)/Makefile.config

TEX_FILES = macros.tex basic_concepts.tex lexical_conventions.tex motivations.tex \
	proofs.tex building_species.tex macros.tex output.tex refman.tex \
	compiler_opts.tex more_on_meths.tex pres_requir.tex \
	constructs_syntax.tex bibli.bib

DOC_OBJS=refman.dvi refman.ps refman.pdf \
  refman.html # Not so great but yet...

TEXINPUT = /usr/share/texmf/tex/latex//:$(TEXINPUTS)
LATEX = TEXINPUTS=$(TEXINPUT); latex
HEVEA = TEXINPUTS=$(TEXINPUT); hevea
IMAGEN = imagen -extra "ppmquant 255"

include $(ROOT_DIR)/Makefile.common

DOCDIR_DIR=$(DOCUMENTATION_DIR)/tex/refman

all doc docdir:: $(DOC_OBJS)

docdir:: $(DOC_OBJS)
	$(MKDIR) $(DOCDIR_DIR) && \
	$(CP) $(DOC_OBJS) $(DOCDIR_DIR)/

refman.dvi: $(TEX_FILES)
	$(LATEX) refman.tex
	makeindex refman
	bibtex refman
	$(LATEX) refman.tex && latex refman.tex

refman.pdf: refman.dvi
	dvipdf refman.dvi

refman.html: refman.dvi
	$(HEVEA) -fix macros.hva refman.hva refman.tex

refman.ps: refman.dvi
	dvips refman.dvi

clean::
	$(RM) *.aux *.log *.pdf refman.ps *.dvi *.idx *.ilg *.ind *.toc

distclean:: clean
	$(RM) $(DOCDIR_DIR)
