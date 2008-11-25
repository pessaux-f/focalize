#  Copyright 2004 INRIA
#  $Id: Makefile,v 1.16 2008-11-25 15:59:15 doligez Exp $

include .config_var

# programs
CAMLC = ocamlc
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc

# flags
DEBUGFLAG	= -g -dtypes
CAMLFLAGS	= -warn-error A
CAMLCFLAGS	= $(CAMLFLAGS) $(DEBUGFLAG) $(XMLRPCFLAGS)

MODULES = version misc options cache \
          expr parser_coq lexer_coq invoke invoke_cime \
          $(XMLRPCFILE) token lexer parser main

LEXERS = lexer_coq lexer
PARSERS = parser_coq

PLAIN_MODULES = version misc cache invoke expr invoke_cime $(XMLRPCFILE) \
                token parser options main

IMPL = $(MODULES:%=%.ml)
INTF = $(MODULES:%=%.mli)
OBJBYT = $(MODULES:%=%.cmo)

.PHONY: all
all: zvtov

zvtov: $(OBJBYT)
	$(CAMLC) $(CAMLCFLAGS) str.cma $(XMLRPCLINK) -o zvtov $(OBJBYT)

.PHONY: install
install:
	mkdir -p "$(DESTDIR)$(bindir)"
	cp zvtov $(DESTDIR)$(bindir)/

.PHONY: uninstall
uninstall:
	rm -f $(bindir)/zvtov

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(CAMLCFLAGS) -c $*.ml

.mli.cmi:
	$(CAMLC) $(CAMLCFLAGS) -c $*.mli

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

lexer_coq.ml: lexer_coq.mll
	$(CAMLLEX) lexer_coq.mll

parser_coq.ml: parser_coq.mly
	$(CAMLYACC) -v parser_coq.mly

parser_coq.mli: parser_coq.ml
	:


.PHONY: clean
clean:
	rm -f *.cmo *.cmi *.annot
	rm -f lexer_coq.ml parser_coq.ml
	rm -f parser_coq.mli
	rm -f zvtov

.PHONY: realclean
realclean: clean
	rm -f .config_var

.PHONY: depend
depend: $(IMPL) $(INTF)
	ocamldep $(IMPL) $(INTF) >.depend

include .depend
