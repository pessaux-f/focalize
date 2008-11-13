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

# $Id: Makefile,v 1.5 2008-11-13 16:28:13 pessaux Exp $

ROOT_DIR = ..

include $(ROOT_DIR)/Makefile.config

TEX_FILES = macros.tex basic_concepts.tex core_lang.tex motivations.tex \
	proofs.tex building_species.tex macros.tex output.tex refman.tex \
	compiler_opts.tex more_on_meths.tex pres_requir.tex \
	constructs_syntax.tex


include $(ROOT_DIR)/Makefile.common

all:: $(TEX_FILES)
	latex refman.tex
	makeindex refman
	latex refman.tex
	dvipdf refman.dvi

clean::
	$(RM) *.aux *.log *.pdf *.ps *.dvi *.idx *.ilg *.ind
