#**********************************************************************#
#                                                                      #
#                        FoCaL compiler                                #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.20 2008-10-14 07:00:37 weis Exp $

ROOT_DIR = ..

CHECK_FILE = $(ROOT_DIR)/toplevel/check_file.byt

OK_TEST_FILES = \
 ok__baby_toy.foc \
 ok__definition_72_rule_PRM.foc \
 ok__in_example.foc \
 ok__in_example2.foc \
 ok__list.foc \
 ok__multiple_inherit.foc \
 ok__need_inspect_self.foc \
 ok__need_re_ordering.foc \
 ok__odd_even.foc \
 ok__phd_def_deps.foc \
 ok__phd_meths_gen.foc \
 ok__phd_sample.foc \
 ok__torture_params.foc
 #ok__coll_outside.foc

KO_TEST_FILES = \
	ko__bad_self_use.foc \
	ko__param_toy.foc \
	ko__test_error.foc \
	ko__test_rec.foc

all:: $(CHECK_FILE) ok_all ko_all

ok_all: $(OK_TEST_FILES)
	# Compile from FoCaL to OCaml.
	for i in $(OK_TEST_FILES); do \
	  echo "Focalizing $$i"; \
	  $(CHECK_FILE) -I ../lib $$i; \
	done; \
	# Compile the generated OCaml code.
	ocamlc -w a -c ok__baby_toy_externals.ml
	for i in $(OK_TEST_FILES:.foc=.ml); do \
	  echo "OCamllizing $$i"; \
	  ocamlc -w a -I ../lib/ \
	    ../lib/ml_builtins.cmo ../lib/basics.cmo \
	    $$i -o $$i.out; \
	done; \
	# Compile the generated Coq code.
	for i in $(OK_TEST_FILES:.foc=.v); do \
	  echo "Coquifying $$i"; \
	  coqc -I ../lib/ $$i; \
	done

ko_all: $(KO_TEST_FILES)
	# Compile from FoCaL to OCaml.
	for i in $(KO_TEST_FILES); do \
	  echo "Focalizing $$i"; \
	  $(CHECK_FILE) -I ../lib $$i; \
	done


clean::
	$(RM) $(OK_TEST_FILES:.foc=.v) $(KO_TEST_FILES:.foc=.v)
	$(RM) $(OK_TEST_FILES:.foc=.ml) $(KO_TEST_FILES:.foc=.ml)

$(CHECK_FILE):
	cd $(ROOT_DIR); $(MAKE) all
