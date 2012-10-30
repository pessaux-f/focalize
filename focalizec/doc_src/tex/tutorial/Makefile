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

# $Id: Makefile,v 1.15 2012-10-30 13:56:05 pessaux Exp $

ROOT_DIR=../../../..

TEX_FILES=tutorial.tex

HVA_FILES=tutorial.hva

FOCALIZE_SRC=superset.fcl subset.fcl mylist.fcl main.fcl

FOCALIZE_FOS=$(FOCALIZE_SRC:.fcl=.fo)
FOCALIZE_MLS=$(FOCALIZE_SRC:.fcl=.ml)

FOCALIZE_CMIS=$(FOCALIZE_SRC:.fcl=.cmi)

FOCALIZE_BYT_OBJS=$(FOCALIZE_MLS:.ml=.cmo)
FOCALIZE_BIN_OBJS=$(FOCALIZE_BYT_OBJS:.cmo=.cmx)

FOCALIZE_ZVS=$(FOCALIZE_SRC:.fcl=.zv)
FOCALIZE_VS=$(FOCALIZE_ZVS:.zv=.v)
FOCALIZE_VOS=$(FOCALIZE_VS:.v=.vo)

FOCALIZE_DOCS=$(FOCALIZE_SRC:.fcl=.fcd)

FOCALIZE_XML=$(FOCALIZE_DOCS:.fcd=.xml)

DOC_OBJS=tutorial.dvi tutorial.ps tutorial.pdf tutorial.html

FOCALIZEC=$(ROOT_DIR)/focalizec/src/toplevel/focalizec
FOCALIZE_COMP_FLAGS=-I $(ROOT_DIR)/focalizec/src/stdlib  $(FOCALIZEC_COQ_COMPATIBILITY_OPTION)
CAML_INCLUDES=-I $(ROOT_DIR)/focalizec/src/stdlib
CAML_COMP_FLAGS=-w Ae -dtypes

# Defining defaults from Makefile.common
include $(ROOT_DIR)/Makefile.common
include $(ROOT_DIR)/focalizec/.install_var
include $(ROOT_DIR)/focalizec/.config_var

# Now overwriting defaults for this directory purposes.
TEXINPUT=/usr/share/texmf/tex/latex//:$(TEXINPUTS)
LATEX=TEXINPUTS=$(TEXINPUT); latex
HEVEA=TEXINPUTS=$(TEXINPUT); hevea
IMAGEN=imagen -extra "ppmquant 255"

all doc: byt $(DOC_OBJS)

byt: $(FOCALIZE_BYT_OBJS)

clean: clean-generated
	$(RM) $(FOCALIZE_MLS) $(FOCALIZE_ZVS) $(FOCALIZE_VS) *.vo *.zv *.cm* *.o

tutorial.dvi: $(TEX_FILES)
	$(LATEX) tutorial.tex &&\
	$(LATEX) tutorial.tex &&\
	$(LATEX) tutorial.tex

tutorial.html: tutorial.dvi ${HVA_FILES}
	$(HEVEA) -fix ${HVA_FILES} tutorial.tex

clean-generated:
	$(RM) *.aux *.log *.dvi *.idx *.ilg *.ind *.toc
	$(RM) *.haux *.htoc
	$(RM) tutorial*.gif tutorial.image.tex
	$(RM) $(DOC_OBJS)
	$(RM) *.cm* *.o *.a *.annot *.out *.output *.bin *.byt build_stamp.ml

install:
	$(MKDIR) $(INSTALL_DOC_DIR)/tutorial &&\
	$(CP) $(DOC_OBJS) $(INSTALL_DOC_DIR)/tutorial/
