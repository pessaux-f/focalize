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

# $Id: Makefile,v 1.4 2011-05-26 10:21:32 weis Exp $

ROOT_DIR=../../..

include $(ROOT_DIR)/Makefile.config

SRC_DIR=$(ROOT_DIR)/src
EXTLIB_SUBDIRS=\
 access_control\
 polynomials\
# algebra\

include $(ROOT_DIR)/Makefile.common

.PHONY: stdlib extlib tests

tests: all

all: stdlib

stdlib:
	mkdir -p stdlib &&\
	($(CD) $(SRC_DIR)/stdlib &&\
	for f in *.fcl; do\
	  ../toplevel/focalizec -no-ocaml-code -no-coq-code\
	    -pretty ../parser/tests/stdlib/$$f\
	    $$f &&\
	  ../toplevel/focalizec -no-ocaml-code -no-coq-code\
	    -pretty ../parser/tests/stdlib/$$f.pretty\
	    ../parser/tests/stdlib/$$f &&\
	  cmp ../parser/tests/stdlib/$$f \
	      ../parser/tests/stdlib/$$f.pretty ||\
	    exit 1;\
	done;\
	)

extlib:
	mkdir -p extlib &&\
	for d in $(EXTLIB_SUBDIRS); do\
	  mkdir -p extlib/$$d &&\
	  ($(CD) $(SRC_DIR)/extlib/$$d &&\
	  for f in *.fcl; do\
	    echo "$$d/$$f -> $$d/$$f" &&\
	    ../../toplevel/focalizec -I ../algebra\
	      -no-ocaml-code -no-coq-code -pretty\
	      ../../parser/tests/extlib/$$d/$$f\
	      $$f &&\
	    echo "$$d/$$f -> $$d/$$f -> $$d/$$f" &&\
	    ../../toplevel/focalizec -I ../algebra -no-ocaml-code\
	      -no-coq-code -pretty \
	      ../../parser/tests/extlib/$$d/$$f.pretty\
	      ../../parser/tests/extlib/$$d/$$f &&\
	    echo "$$d/$$f -> $$d/$$f=$$d/$$f" &&\
	    cmp ../../parser/tests/extlib/$$d/$$f \
	        ../../parser/tests/extlib/$$d/$$f.pretty ||\
	    exit 1;\
	  done;\
	  )\
	done;
