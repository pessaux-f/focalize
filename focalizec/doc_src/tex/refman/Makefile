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

# $Id: Makefile,v 1.3 2008-11-04 12:27:10 pessaux Exp $

ROOT_DIR = ..

include $(ROOT_DIR)/Makefile.config

TEX_FILES = basic_concepts.tex \
	core_lang.tex \
	motivations.tex \
	proofs.tex \
	building_species.tex \
	macros.tex \
	output.tex \
	refman.tex \
	compiler_opts.tex\
	more_on_meths.tex \
	pres_requir.tex

DVI_FILE = refman.dvi
PS_FILE = $(DVI_FILE:.dvi=.ps)
PDF_FILE = $(DVI_FILE:.dvi=.pdf)
#IDX_FILES = $(TEX_FILES:.tex=.idx)


include $(ROOT_DIR)/Makefile.common

all byt bin:: $(PDF_FILE)

$(DVI_FILE): $(TEX_FILES)

clean::
	$(RM) *.aux *.log *.pdf *.ps *.dvi *.idx *.ilg *.ind
