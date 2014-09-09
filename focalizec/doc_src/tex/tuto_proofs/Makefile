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


FOCALIZE_SRC= ex_implications.fcl or_id_com_good.fcl or_id_com_shortest.fcl same_comps.fcl diff_comps.fcl signal2.fcl signal3.fcl controller/controller_algo.fcl controller/controller_complete.fcl controller/controller1.fcl controller/controller2.fcl controller/controller3.fcl controller/controller5.fcl controller/controller5.fcl lemmas1.fcl lemmas3.fcl lemmas4.fcl lemmas5.fcl

# Particular rule because Zenon has a missing case when outputing proofs
# as a Coq term. And this source file falls in the hole ^_^
FOCALIZE_SRC2=tree_mirror2.fcl tree_mirror2bis.fcl tree_mirror.fcl zero3.fcl

FOCALIZE_MLS=$(FOCALIZE_SRC:.fcl=.ml)
FOCALIZE_BYT_OBJS=$(FOCALIZE_MLS:.ml=.cmo) $(FOCALIZE_SRC2:.fcl=.vo)

TEX_FILES = conclusion.tex first_order_logic.tex intro.tex tuto_proofs.tex init.tex	program.tex bibli.bib

HVA_FILES=tuto_proofs.hva

include $(ROOT_DIR)/focalizec/.config_var
ifeq ($(HEVEAFOUND),true)
DOC_OBJS=tuto_proofs.dvi tuto_proofs.ps tuto_proofs.pdf tuto_proofs.html
else
DOC_OBJS=tuto_proofs.dvi tuto_proofs.ps tuto_proofs.pdf
endif

FOCALIZEC=$(ROOT_DIR)/focalizec/src/toplevel/focalizec
FOCALIZE_COMP_FLAGS= -I $(ROOT_DIR)/focalizec/src/stdlib  $(FOCALIZEC_COQ_COMPATIBILITY_OPTION)
CAML_INCLUDES=-I $(ROOT_DIR)/focalizec/src/stdlib
CAML_COMP_FLAGS=-w Ae -dtypes

# Defining defaults from Makefile.common
include $(ROOT_DIR)/Makefile.common
include $(ROOT_DIR)/focalizec/.install_var

# Now overwriting defaults for this directory purposes.
TEXINPUT=/usr/share/texmf/tex/latex//:$(TEXINPUTS)
LATEX=TEXINPUTS=$(TEXINPUT); latex
HEVEA=TEXINPUTS=$(TEXINPUT); hevea


# Only try to build doc if source files are present.
all doc:
	@if test -f tuto_proofs.tex; then make byt ; make $(DOC_OBJS); fi

byt: $(FOCALIZE_BYT_OBJS)


# Particular rules because Zenon has a missing case when outputing proofs
# as a Coq term. And this source file falls in the hole ^_^
tree_mirror2.vo: tree_mirror2.fcl
	$(FOCALIZEC) $(FOCALIZE_COMP_FLAGS) -zvtovopt -script tree_mirror2.fcl
tree_mirror2bis.vo: tree_mirror2bis.fcl
	$(FOCALIZEC) $(FOCALIZE_COMP_FLAGS) -zvtovopt -script tree_mirror2bis.fcl
tree_mirror.vo: tree_mirror.fcl
	$(FOCALIZEC) $(FOCALIZE_COMP_FLAGS) -zvtovopt -script tree_mirror.fcl
zero3.vo: zero3.fcl
	$(FOCALIZEC) $(FOCALIZE_COMP_FLAGS) -zvtovopt -script zero3.fcl


tuto_proofs.dvi: $(TEX_FILES)
	$(LATEX) tuto_proofs.tex
	bibtex tuto_proofs
	$(LATEX) tuto_proofs.tex && $(LATEX) tuto_proofs.tex

tuto_proofs.html: tuto_proofs.dvi ${HVA_FILES}
	$(HEVEA) -fix ${HVA_FILES} tuto_proofs.tex

tuto_proofs.pdf: tuto_proofs.dvi
	$(DVIPDF) tuto_proofs.dvi tuto_proofs.pdf

# Only remove doc files if sources are present, hence doc can be rebuilt.
clean:
	$(RM) *.ml *.fo *.cm* *.v *.vo *.pfc *.zvtmp.pfc *.glob *.zv *.log *.aux
	($(CD) controller &&\
	$(RM) *.ml *.fo *.cm* *.v *.vo *.pfc *.zvtmp.pfc *.glob *.zv *.log *.aux &&\
	$(CD) ..)
	@if test -f tuto_proofs.tex; then $(RM) $(DOC_OBJS); fi

install:
	$(MKDIR) $(INSTALL_DOC_DIR)/tuto_proofs &&\
	$(CP) $(DOC_OBJS) $(INSTALL_DOC_DIR)/tuto_proofs/
