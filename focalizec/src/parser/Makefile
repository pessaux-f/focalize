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

# $Id: Makefile,v 1.43 2011-05-26 10:21:32 weis Exp $

ROOT_DIR=../..

include $(ROOT_DIR)/Makefile.config

CAML_INCLUDES=-I ../basement

BYT_OBJS=lexer.cmo parser.cmo parse_file.cmo \
 dump_ptree.cmo sourcify.cmo sourcify_token.cmo lex_file.cmo

include $(ROOT_DIR)/Makefile.common

clean-local:
	$(RM) lexer.ml lexer.mli
	$(RM) parser.ml parser.mli parser.conflicts parser.output
	$(RM) menhir.out
	$(RM) lex_file
	#$(MAKE) depend

depend-local: lexer.ml parser.ml
	$(CAML_DEP) *.mli *.ml > ./.depend

explain:
	menhir --explain parser.mly > menhir.out 2>&1

lex_file: $(BYT_OBJS)
	$(CAML_BYT) -o lex_file lexer.cmo sourcify_token.cmo lex_file.cmo

parser.mli: parser.ml
parser.cmi: parser.mli
