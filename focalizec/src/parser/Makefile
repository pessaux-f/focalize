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

# $Id: Makefile,v 1.38 2008-10-22 09:07:16 weis Exp $

ROOT_DIR = ..

include $(ROOT_DIR)/Makefile.config

CAML_INCLUDES = -I ../basement

BYTOBJS = lexer.cmo parser.cmo parse_file.cmo \
 dump_ptree.cmo sourcify.cmo sourcify_token.cmo lex_file.cmo

include $(ROOT_DIR)/Makefile.common

clean::
	$(RM) lexer.ml lexer.mli
	$(RM) parser.ml parser.mli parser.conflicts parser.output
	$(RM) menhir.out
	$(MAKE) depend

depend:: lexer.ml parser.ml
	$(CAMLDEP) *.mli *.ml > ./.depend

explain:
	menhir --explain parser.mly > menhir.out 2>&1

clean::
	$(RM) lex_file
lex_file: $(BYTOBJS)
	$(CAMLBYT) -o lex_file lexer.cmo sourcify_token.cmo lex_file.cmo

