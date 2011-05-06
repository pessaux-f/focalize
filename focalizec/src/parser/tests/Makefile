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

# $Id: Makefile,v 1.2 2011-05-06 18:01:39 maarek Exp $

ROOT_DIR = ../../..

include $(ROOT_DIR)/Makefile.config

HERE_DIR=src/parser/tests
PRETTY_DIR=$(ROOT_DIR)/$(HERE_DIR)
SRC_DIR=$(ROOT_DIR)/src
EXTLIB_SUBDIRS =\
 access_control\
 polynomials\
 algebra\

include $(ROOT_DIR)/Makefile.common

.PHONY: stdlib tests
tests:: all




all:: stdlib

stdlib:
	mkdir -p stdlib &&\
	($(CD) $(SRC_DIR)/stdlib &&\
	for f in *.fcl; do\
	  ../toplevel/focalizec -no-ocaml-code -no-coq-code \
	  -pretty ../parser/tests/stdlib/$$f \
	  $$f && \
	  ../toplevel/focalizec -no-ocaml-code -no-coq-code \
	  -pretty ../parser/tests/stdlib/$$f.pretty \
	  ../parser/tests/stdlib/$$f && \
	  cmp ../parser/tests/stdlib/$$f ../parser/tests/stdlib/$$f.pretty ||\
	  exit 1;\
	done;\
	)
#extlib:
#	



	# ($(CD) $(ROOT_DIR) && \
	# pwd;\
	# for d in $(TEST_DIRS); do \
	#   mkdir -p $(PRETTY_DIR)/$$d && \
	#   ($(CD) $$d &&
	#   for f in $(SRC_DIR)/$$d/*.fcl; do \
	#      src/toplevel/$(FOCALIZEC) -no-ocaml-code -no-coq-code \
	#        -pretty $(PRETTY_DIR)/$$d/$$f \
	#        $$f && \
	#      src/toplevel/$(FOCALIZEC) -no-ocaml-code -no-coq-code \
        #        -pretty $(PRETTY_DIR)/$$d/$$f.pretty \
	#        $(PRETTY_DIR)/$$d/$$f && \
	#      cmp $(PRETTY_DIR)/$$d/$$f $(PRETTY_DIR)/$$d/$$f.pretty ||\
	#      exit 1;\
	#   done;\
	# done;\
	# )


