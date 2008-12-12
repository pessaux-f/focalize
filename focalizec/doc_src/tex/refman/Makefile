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

# $Id: Makefile,v 1.8 2008-12-12 08:11:38 weis Exp $

ROOT_DIR = ../../../..

include $(ROOT_DIR)/Makefile.config

TEX_FILES = macros.tex basic_concepts.tex lexical_conventions.tex motivations.tex \
	proofs.tex building_species.tex macros.tex output.tex refman.tex \
	compiler_opts.tex more_on_meths.tex pres_requir.tex \
	constructs_syntax.tex bibli.bib


include $(ROOT_DIR)/Makefile.common

all:: $(TEX_FILES)
	latex refman.tex
	makeindex refman
	bibtex refman
	latex refman.tex
	dvipdf refman.dvi

clean::
	$(RM) *.aux *.log *.pdf *.ps *.dvi *.idx *.ilg *.ind
