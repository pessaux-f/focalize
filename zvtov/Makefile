#**********************************************************************#
#                                                                      #
#                        FoCaLize compiler                             #
#                                                                      #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                                                                      #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2004, 2008 INRIA                                          #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

#  $Id: Makefile,v 1.23 2008-12-27 19:05:40 weis Exp $

include .config_var

# programs defined in .config_var
#CAMLBYT = ocamlc
#CAMLBIN = ocamlopt
#CAMLLEX = ocamllex
#CAMLYACC = ocamlyacc

# flags
DEBUGFLAG	= -g -dtypes
COMPFLAGS	= -warn-error A
CAMLBYTFLAGS	= $(COMPFLAGS) $(DEBUGFLAG) $(XMLRPCFLAGS)
CAMLBINFLAGS	= $(COMPFLAGS) $(DEBUGFLAG) $(XMLRPCFLAGS)

MODULES = version misc options cache \
          expr parser_coq lexer_coq invoke invoke_cime \
          $(XMLRPC_MODULE) token lexer parser main

LEXERS = lexer_coq lexer
PARSERS = parser_coq

PLAIN_MODULES = version misc cache invoke expr invoke_cime $(XMLRPC_MODULE) \
                token parser options main

IMPL = $(MODULES:%=%.ml)
INTF = $(MODULES:%=%.mli)
BYTOBJS = $(MODULES:%=%.cmo)
BINOBJS = $(MODULES:%=%.cmx)

.PHONY: all byt bin

all: byt bin zvtov

byt: $(BYTOBJS)
	$(CAMLBYT) $(CAMLBYTFLAGS) str.cma $(XMLRPCLINK) -o zvtov.byt $(BYTOBJS)
	cp -p zvtov.byt zvtov

bin: $(BINOBJS)
	$(CAMLBIN) $(CAMLBINFLAGS) str.cmxa $(XMLRPCLINK) -o zvtov.bin $(BINOBJS)
	cp -p zvtov.bin zvtov

zvtov: zvtov.byt
	if test -x zvtov.bin; then \
	  cp zvtov.bin zvtov; \
        else \
	  cp zvtov.byt zvtov; \
	fi

.PHONY: install
install:
	mkdir -p "$(BIN_DIR)"
	cp -p zvtov "$(BIN_DIR)"/
	mkdir -p "$(MAN_DIR)"/man1
	cp zvtov.1 "$(MAN_DIR)"/man1/

.PHONY: uninstall
uninstall:
	rm -f "$(BIN_DIR)"/zvtov
	rm -f "$(MAN_DIR)"/man1/zvtov.1

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
