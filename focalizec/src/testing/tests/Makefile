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

# $Id: Makefile,v 1.1 2011-06-14 13:59:43 maarek Exp $

ROOT_DIR = ../../..

include $(ROOT_DIR)/Makefile.config

include $(ROOT_DIR)/Makefile.common

.PHONY: tests
tests:: all

all:: interf compile pretty

compile::
	~/Work/focalize/focalize/focalizec/src/toplevel/focalizec -verbose -no-coq-code length.fcl
	~/Work/focalize/focalize/focalizec/src/toplevel/focalizec -verbose -no-coq-code triangle.fcl

interf::
	~/Work/focalize/focalize/focalizec/src/toplevel/focalizec -i -no-ocaml-code -no-coq-code length.fcl
	~/Work/focalize/focalize/focalizec/src/toplevel/focalizec -i -no-ocaml-code -no-coq-code triangle.fcl

pretty::
	~/Work/focalize/focalize/focalizec/src/toplevel/focalizec \
           -no-ocaml-code -no-coq-code -pretty length.pretty.fcl length.fcl
	~/Work/focalize/focalize/focalizec/src/toplevel/focalizec \
           -no-ocaml-code -no-coq-code -pretty length.pretty.pretty.fcl length.pretty.fcl
	cmp length.pretty.fcl length.pretty.pretty.fcl
	~/Work/focalize/focalize/focalizec/src/toplevel/focalizec \
           -no-ocaml-code -no-coq-code -pretty triangle.pretty.fcl triangle.fcl
	~/Work/focalize/focalize/focalizec/src/toplevel/focalizec \
           -no-ocaml-code -no-coq-code -pretty triangle.pretty.pretty.fcl triangle.pretty.fcl
	cmp triangle.pretty.fcl triangle.pretty.pretty.fcl

clean::
	rm -f *.cmi *.cmo *.fo *.pfc *.v *.zv *.ml *.vo *.zv.mangled
	rm -f *_TESTS.*
	rm -f *.pretty.*

