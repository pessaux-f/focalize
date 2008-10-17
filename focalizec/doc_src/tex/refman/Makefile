#**********************************************************************#
#                                                                      #
#                        FoCaL compiler                                #
#            François Pessaux                                          #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.1 2008-10-17 11:13:16 pessaux Exp $

LATEX = latex
MAKEINDEX = makeindex
DVIPDF = dvipdf

TEXFILE = refman.tex
DVIFILE = $(TEXFILE:.tex=.dvi)
IDXFILE = $(TEXFILE:.tex=.idx)


all: $(TEXFILE)
	$(LATEX) $(TEXFILE)
	$(MAKEINDEX) $(IDXFILE)
	$(LATEX) $(TEXFILE)
	$(DVIPDF) $(DVIFILE)



clean:
	rm -f *~ *.aux *.log *.pdf *.dvi *.idx *.ilg *.ind
