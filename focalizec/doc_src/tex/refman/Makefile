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

ROOT_DIR=../../../..

TEX_FILES= syntaxdef.sty\
	basic_concepts.tex glimpse.tex output.tex building_species.tex \
	introduction.tex pres_requir.tex compiler_err_msgs.tex \
	lexical_conventions.tex proofs.tex compiler_opts.tex macros.tex \
	recursion.tex constructs_syntax.tex more_on_meths.tex \
	doc_gen.tex	motivations.tex	refman.tex bibli.bib

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
