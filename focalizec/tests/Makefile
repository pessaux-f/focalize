#**********************************************************************#
#                                                                      #
#                        FoCaLize compiler                             #
#                                                                      #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.27 2011-05-27 14:35:45 weis Exp $

ROOT_DIR=..

include $(ROOT_DIR)/Makefile.config

CHECK_FILE=$(ROOT_DIR)/src/toplevel/focalizec

OK_TEST_FILES=\
 ok__baby_toy.fcl \
 ok__definition_72_rule_PRM.fcl \
 ok__in_example.fcl \
 ok__in_example2.fcl \
 ok__list.fcl \
 ok__multiple_inherit.fcl \
 ok__need_inspect_self.fcl \
 ok__need_re_ordering.fcl \
 ok__odd_even.fcl \
 ok__phd_def_deps.fcl \
 ok__phd_meths_gen.fcl \
 ok__phd_sample.fcl \
 ok__torture_params.fcl
 #ok__coll_outside.fcl

KO_TEST_FILES=\
 ko__bad_self_use.fcl \
 ko__param_toy.fcl \
 ko__test_error.fcl \
 ko__test_rec.fcl

include $(ROOT_DIR)/Makefile.common

all: $(CHECK_FILE) ok_all ko_all

ok_all: $(OK_TEST_FILES)
	# Compile from Focalize to OCaml.
	@for i in $(OK_TEST_FILES); do \
	  echo "Focalizing $$i"; \
	  $(CHECK_FILE) -I ../lib $$i; \
	done; \
	# Compile the generated OCaml code.
	ocamlc -w a -c ok__baby_toy_externals.ml
	for i in $(OK_TEST_FILES:.fcl=.ml); do \
	  echo "OCamllizing $$i"; \
	  ocamlc -w a -I ../lib/ \
	    ../lib/ml_builtins.cmo ../lib/basics.cmo \
	    $$i -o $$i.out; \
	done; \
	# Compile the generated Coq code.
	for i in $(OK_TEST_FILES:.fcl=.v); do \
	  echo "Coquifying $$i"; \
	  coqc -I ../lib/ $$i; \
	done

ko_all: $(KO_TEST_FILES)
	# Compile from Focalize to OCaml.
	@for i in $(KO_TEST_FILES); do \
	  echo "Focalizing $$i"; \
	  $(CHECK_FILE) -I ../lib $$i; \
	done

clean:
	$(RM) $(OK_TEST_FILES:.fcl=.v) $(KO_TEST_FILES:.fcl=.v)
	$(RM) $(OK_TEST_FILES:.fcl=.ml) $(KO_TEST_FILES:.fcl=.ml)
	$(RM) *.annot *.glob *.pfc *.zv *~
