# ************************************************************************** #
#                                                                            #
#                        FoCaLiZe compiler                                   #
#                                                                            #
#            François Pessaux                                                #
#            Pierre Weis                                                     #
#            Damien Doligez                                                  #
#                                                                            #
#                 LIP6  --  INRIA Rocquencourt - ENSTA ParisTech             #
#                                                                            #
#  Copyright 2007 - ... LIP6 and INRIA                                       #
#            2012 - ... ENSTA ParisTech                                      #
#  Distributed only by permission.                                           #
#                                                                            #
# ************************************************************************** #

ROOT_DIR=../..

CHECKED_FOCALIZEC=$(ROOT_DIR)/focalizec/src/toplevel/focalizec
CHECKED_STDLIB=$(ROOT_DIR)/focalizec/src/stdlib

OK_TEST_FILES=\
  ok__baby_toy.fcl ok__caveat.fcl ok__coll_outside.fcl \
	ok__definition_72_rule_PRM.fcl ok__in_example.fcl ok__in_example2.fcl \
  ok__list.fcl ok__multiple_inherit.fcl ok__need_inspect_self.fcl \
	ok__need_re_ordering.fcl ok__odd_even.fcl ok__phd_def_deps.fcl \
	ok__phd_meths_gen.fcl ok__phd_sample.fcl ok__pmatch_checks.fcl \
	ok__scoping_tricky.fcl ok__term_measure.fcl ok__toplevel_odd_even.fcl \
	ok__torture_params.fcl ok__torture_params2.fcl ok__torture_params3.fcl


KO_TEST_FILES = \
  ko__bad_self_use.fcl  ko__param_toy.fcl ko__test_error.fcl ko__test_rec.fcl


include $(ROOT_DIR)/Makefile.common

all: $(CHECKED_FOCALIZEC) ok_all ko_all

ok_all: $(OK_TEST_FILES)
	# Compile from Focalize to OCaml.
	$(COQC) ok__baby_toy_coq_externals.v
	$(CAMLBYT) ok__baby_toy_externals.ml
	@for i in $(OK_TEST_FILES); do \
	  echo "Focalizing $$i"; \
	  $(CHECKED_FOCALIZEC) -I $(CHECKED_STDLIB) $$i; \
	done

ko_all: $(KO_TEST_FILES)
	# Compile from Focalize to OCaml.
	@for i in $(KO_TEST_FILES); do \
	  echo "Focalizing $$i"; \
	  $(CHECKED_FOCALIZEC) -I $(CHECKED_STDLIB) $$i; \
	done

clean:
	$(RM) $(OK_TEST_FILES:.fcl=.v) $(OK_TEST_FILES:.fcl=.ml)
	$(RM) *.annot *.glob *.pfc *.zv *~ *.vo *.cm* *.fo
