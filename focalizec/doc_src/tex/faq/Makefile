#                                                                            #
#            François Pessaux                                                #
#                                                                            #
#                 LIP6  --  INRIA Rocquencourt - ENSTA ParisTech             #
#                                                                            #
#  Copyright 2007 - ... LIP6 and INRIA                                       #
#            2012 - ... ENSTA ParisTech                                      #
#  Distributed only by permission.                                           #
#                                                                            #
# ************************************************************************** #

ROOT_DIR=../../../..
TEX_FILES = macros.tex faq.tex

HVA_FILES=faq.hva

DOC_OBJS=faq.dvi faq.ps faq.pdf faq.html

# Defining defaults from Makefile.common
include $(ROOT_DIR)/Makefile.common
include $(ROOT_DIR)/focalizec/.install_var
include $(ROOT_DIR)/focalizec/.config_var

# Now overwriting defaults for this directory purposes.
TEXINPUT=/usr/share/texmf/tex/latex//:$(TEXINPUTS)
LATEX=TEXINPUTS=$(TEXINPUT); latex
HEVEA=TEXINPUTS=$(TEXINPUT); hevea


# Only try to build doc if source files are present.
all doc:
	@if test -f faq.tex; then make $(DOC_OBJS); fi


faq.dvi: $(TEX_FILES)
	$(LATEX) faq.tex

faq.html: faq.dvi ${HVA_FILES}
	$(HEVEA) -fix ${HVA_FILES} faq.tex

faq.pdf: faq.dvi
	$(DVIPDF) faq.dvi faq.pdf

clean:
	$(RM) *.log *.aux $(DOC_OBJS) *~
