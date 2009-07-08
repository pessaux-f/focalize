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

# $Id: Makefile,v 1.25 2009-07-07 22:35:01 weis Exp $

ROOT_DIR = ../../..

include $(ROOT_DIR)/Makefile.config

TEX_FILES = macros.tex syntaxdef.sty \
  basic_concepts.tex lexical_conventions.tex motivations.tex\
  proofs.tex building_species.tex macros.tex output.tex refman.tex\
  compiler_opts.tex more_on_meths.tex pres_requir.tex\
  constructs_syntax.tex bibli.bib

HVA_FILES = macros.hva syntaxdef.hva refman.hva

DOC_OBJS = refman.dvi refman.ps refman.pdf refman.html

TEXINPUT = /usr/share/texmf/tex/latex//:$(TEXINPUTS)
LATEX = TEXINPUTS=$(TEXINPUT); latex
HEVEA = TEXINPUTS=$(TEXINPUT); hevea
IMAGEN = imagen -extra "ppmquant 255"

include $(ROOT_DIR)/Makefile.common

# DOCDIR_DIR is the directory doc that this Makefile must populate.
DOCDIR_DIR=$(DOC_ROOT_DIR)/tex/refman

# DOC_TGT_DIR is the target directory for the installation of the documentation
DOC_TGT_DIR:=$(DOC_DIR)/tex/$(PROJECT_NAME_FULL)/refman

all doc docdir:: $(DOC_OBJS)

docdir:: $(DOC_OBJS)
	$(MKDIR) $(DOCDIR_DIR) && \
	$(CP) $(DOC_OBJS) $(DOCDIR_DIR)/

refman.dvi: $(TEX_FILES)
	$(LATEX) refman.tex
	makeindex refman
	bibtex refman
	$(LATEX) refman.tex && $(LATEX) refman.tex

refman.pdf: refman.dvi
	dvipdf refman.dvi

refman.html: refman.dvi ${HVA_FILES}
	$(HEVEA) -fix ${HVA_FILES} refman.tex

refman.ps: refman.dvi
	dvips refman.dvi

clean::
	$(RM) *.aux *.log *.dvi *.idx *.ilg *.ind *.toc
	$(RM) refman*.gif refman.image.tex
	$(RM) $(DOC_OBJS)

distclean:: clean
	$(RM) $(DOCDIR_DIR)

install:
	$(MKDIR) $(DOC_TGT_DIR) && \
	$(CP) $(DOC_OBJS) $(DOC_TGT_DIR)

uninstall:
	$(RM) $(DOC_TGT_DIR)
