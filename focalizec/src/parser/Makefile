# $Id: Makefile,v 1.32 2008-09-18 22:39:00 weis Exp $

ROOTDIR = ..

include $(ROOTDIR)/Makefile.config

CAML_COMP_FLAGS = -I ../basement

BYTOBJS = lexer.cmo parser.cmo parse_file.cmo dump_ptree.cmo sourcify.cmo

include $(ROOTDIR)/Makefile.common

all:: lexer.ml parser.ml

clean::
	$(RM) lexer.ml lexer.mli
	$(RM) parser.ml parser.mli parser.conflicts parser.output

depend:: lexer.ml parser.ml
	$(CAMLDEP) $(CAML_COMP_FLAGS) *.mli *.ml > ./.depend

explain:
	menhir --explain parser.mly > menhir.out 2>&1

lexer.cmi: ../basement/parsetree.cmi parser.cmi lexer.ml

parser.cmi: parser.ml

parser.mli: parser.ml
