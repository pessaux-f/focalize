#**********************************************************************#
#                                                                      #
#                        FoCaLize compiler                             #
#                                                                      #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2004, 2008 INRIA                                          #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

#  $Id: Makefile,v 1.17 2008-11-30 11:46:31 weis Exp $

include .config_var

# programs
CAMLBYT = ocamlc
CAMLBIN = ocamlopt
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc

# flags
DEBUGFLAG	= -g -dtypes
COMPFLAGS	= -warn-error A
CAMLBYTFLAGS	= $(COMPFLAGS) $(DEBUGFLAG) $(XMLRPCFLAGS)
CAMLBINFLAGS	= $(COMPFLAGS) $(DEBUGFLAG) $(XMLRPCFLAGS)

MODULES = version misc options cache \
          expr parser_coq lexer_coq invoke invoke_cime \
          $(XMLRPCFILE) token lexer parser main

LEXERS = lexer_coq lexer
PARSERS = parser_coq

PLAIN_MODULES = version misc cache invoke expr invoke_cime $(XMLRPCFILE) \
                token parser options main

IMPL = $(MODULES:%=%.ml)
INTF = $(MODULES:%=%.mli)
BYTOBJS = $(MODULES:%=%.cmo)
BINOBJS = $(MODULES:%=%.cmx)

.PHONY: all

all: byt bin

byt: $(BYTOBJS)
	$(CAMLBYT) $(CAMLBYTFLAGS) str.cma $(XMLRPCLINK) -o zvtov.byt $(BYTOBJS)
	cp -p zvtov.byt zvtov

bin: $(BINOBJS)
	$(CAMLBIN) $(CAMLBINFLAGS) str.cmxa $(XMLRPCLINK) -o zvtov.bin $(BINOBJS)
	cp -p zvtov.bin zvtov

.PHONY: install
install:
	mkdir -p "$(bindir)"
	cp -p zvtov "$(bindir)"/

.PHONY: uninstall
uninstall:
	rm -f "$(bindir)"/zvtov

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLBYT) $(CAMLBYTFLAGS) -c $*.ml

.ml.cmx:
	$(CAMLBIN) $(CAMLBINFLAGS) -c $*.ml

.mli.cmi:
	$(CAMLBYT) $(CAMLBYTFLAGS) -c $*.mli

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
	rm -f *.cm* *.o *.annot
	rm -f lexer_coq.ml parser_coq.ml
	rm -f parser_coq.mli
	rm -f zvtov *.bin *.byt

.PHONY: realclean
realclean: clean
	rm -f .config_var

.PHONY: depend
depend: $(IMPL) $(INTF)
	ocamldep $(IMPL) $(INTF) >.depend

include .depend
