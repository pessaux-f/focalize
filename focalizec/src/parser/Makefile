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

# $Id: Makefile,v 1.35 2008-10-14 12:10:22 weis Exp $

ROOT_DIR = ..

include $(ROOT_DIR)/Makefile.config

CAML_INCLUDES = -I ../basement

BYTOBJS = lexer.cmo parser.cmo parse_file.cmo dump_ptree.cmo sourcify.cmo

include $(ROOT_DIR)/Makefile.common

all:: lexer.ml parser.ml

clean::
	$(RM) lexer.ml lexer.mli
	$(RM) parser.ml parser.mli parser.conflicts parser.output

depend:: lexer.ml parser.ml
	$(CAMLDEP) *.mli *.ml > ./.depend

explain:
	menhir --explain parser.mly > menhir.out 2>&1

lexer.cmi: ../basement/parsetree.cmi parser.cmi lexer.ml

parser.cmi: parser.ml

parser.mli: parser.ml
