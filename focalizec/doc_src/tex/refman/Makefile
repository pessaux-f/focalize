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

# $Id: Makefile,v 1.2 2008-10-28 16:19:07 weis Exp $

ROOT_DIR = ..

include $(ROOT_DIR)/Makefile.config

TEX_FILES = refman.tex
DVI_FILES = $(TEX_FILES:.tex=.dvi)
PS_FILES = $(DVI_FILES:.dvi=.ps)
PDF_FILES = $(DVI_FILES:.dvi=.pdf)
#IDX_FILES = $(TEX_FILES:.tex=.idx)

BYT_OBJS = $(DVI_FILES) $(PS_FILES) $(PDF_FILES)
BIN_OBJS = $(BYT_OBJS)

include $(ROOT_DIR)/Makefile.common

all byt bin:: $(TEX_FILES)

refman.dvi: macros.tex

clean::
	$(RM) *.aux *.log *.pdf *.ps *.dvi *.idx *.ilg *.ind
