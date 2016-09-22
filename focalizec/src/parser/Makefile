# ************************************************************************** #
#                                                                            #
#                        FoCaLiZe compiler                                   #
#                                                                            #
#            François Pessaux                                                #
#            Pierre Weis                                                     #
#            Damien Doligez                                                  #
#                                                                            #
#                               LIP6  --  INRIA Rocquencourt                 #
#                                                                            #
#  Copyright 2007 - ... LIP6 and INRIA                                       #
#            2012 - ... ENSTA ParisTech                                      #
#  Distributed only by permission.                                           #
#                                                                            #
# ************************************************************************** #


ROOT_DIR=../../..

# Must be defined before including Makefile.common since is used by it.
BYT_OBJS=lexer.cmo parser.cmo parse_file.cmo \
 dump_ptree.cmo sourcify.cmo sourcify_token.cmo lex_file.cmo

include $(ROOT_DIR)/Makefile.common

CAML_INCLUDES=-I ../basement
CAML_FLAGS = -w A-4-9 -warn-error $(WARN_ERROR) -safe-string

depend: lexer.ml parser.ml
	$(CAMLDEP) $(CAML_INCLUDES) *.mli *.ml > ./.depend

explain:
	menhir --explain parser.mly > menhir.out 2>&1

lex_file: $(BYT_OBJS)
	$(CAMLBYT) $(CAML_INCLUDES) -o lex_file lexer.cmo sourcify_token.cmo lex_file.cmo

install: # Nothing to install from there.

uninstall: # Nothing to install from there.

clean:
	$(RM) lexer.ml lexer.mli
	$(RM) parser.ml parser.mli parser.conflicts parser.output
	$(RM) menhir.out
	$(RM) lex_file
	$(RM) *.cm* *.o *.a *.annot *.out *.output *.bin *.byt *~ .*~


include .depend
