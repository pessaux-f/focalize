# ************************************************************************** #
#                                                                            #
#                        FoCaLiZe compiler                                   #
#                                                                            #
#            François Pessaux                                                #
#                                                                            #
#                               LIP6  --  INRIA Rocquencourt                 #
#                                                                            #
#  Copyright 2007 - 2012 LIP6 and INRIA                                      #
#            2012 ENSTA ParisTech                                            #
#  Distributed only by permission.                                           #
#                                                                            #
# ************************************************************************** #

# $Id: Makefile,v 1.30 2012-10-30 13:56:05 pessaux Exp $

ROOT_DIR=../../../..

TEX_FILES=macros.tex syntaxdef.sty\
 basic_concepts.tex lexical_conventions.tex motivations.tex\
 proofs.tex building_species.tex macros.tex output.tex refman.tex\
 compiler_opts.tex more_on_meths.tex pres_requir.tex\
 constructs_syntax.tex bibli.bib

HVA_FILES=macros.hva syntaxdef.hva refman.hva

DOC_OBJS=refman.dvi refman.ps refman.pdf refman.html

TEXINPUT=/usr/share/texmf/tex/latex//:$(TEXINPUTS)
LATEX=TEXINPUTS=$(TEXINPUT); latex
HEVEA=TEXINPUTS=$(TEXINPUT); hevea
IMAGEN=imagen -extra "ppmquant 255"

include $(ROOT_DIR)/Makefile.common
include $(ROOT_DIR)/focalizec/.install_var

all: doc

doc: $(DOC_OBJS)

refman.pdf: refman.dvi
	$(DVIPDF) refman.dvi refman.pdf

refman.dvi: $(TEX_FILES)
	$(LATEX) refman.tex &&\
	makeindex refman &&\
	bibtex refman &&\
	$(LATEX) refman.tex && $(LATEX) refman.tex

refman.html: refman.dvi ${HVA_FILES}
	$(HEVEA) -fix ${HVA_FILES} refman.tex

clean:
	$(RM) *.aux *.log *.dvi *.idx *.ilg *.ind *.toc *.blg *.bbl *.haux *.htoc *.hind
	$(RM) refman*.gif refman.image.tex
	$(RM) $(DOC_OBJS)
	$(RM) *.cm* *.o *.a *.annot *.out *.output *.bin *.byt

install:
	$(MKDIR) $(INSTALL_DOC_DIR)/refman &&\
	$(CP) $(DOC_OBJS) $(INSTALL_DOC_DIR)/refman/
