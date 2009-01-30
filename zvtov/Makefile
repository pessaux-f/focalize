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

#  $Id: Makefile,v 1.28 2009-01-30 11:32:24 weis Exp $

# Reading configuration settings.
include .config_var

# Staging directory for package managers
DESTDIR = ""

CAMLFLAGS = -warn-error A

# Variables CAMLBYT, CAMLBIN, CAMLLEX, CAMLYACC, CAMLDEP and CAMLDOC are defined
# at configuration time their value is recorded in .config_var.
CAMLBINFLAGS = $(CAMLFLAGS) $(BIN_DEBUG_FLAGS) $(XMLRPC_FLAGS)

CAMLBYTFLAGS = $(CAMLFLAGS) $(BYT_DEBUG_FLAGS) $(XMLRPC_FLAGS)

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

byt: zvtov.byt

bin: zvtov.bin

zvtov.bin: $(BINOBJS)
	$(CAMLBIN) $(CAMLBINFLAGS) str.cmxa $(XMLRPC_LINK) -o zvtov.bin $(BINOBJS)

zvtov.byt: $(BYTOBJS)
	$(CAMLBYT) $(CAMLBYTFLAGS) str.cma $(XMLRPC_LINK) -o zvtov.byt $(BYTOBJS)

zvtov: zvtov.byt
	if test -x zvtov.bin; then \
	  cp zvtov.bin zvtov; \
        else \
	  cp zvtov.byt zvtov; \
	fi

.PHONY: install
install:
	mkdir -p "$(DESTDIR)$(BIN_DIR)"
	cp -p zvtov "$(DESTDIR)$(BIN_DIR)"/
	mkdir -p "$(DESTDIR)$(MAN_DIR)"/man1
	cp zvtov.1 "$(DESTDIR)$(MAN_DIR)"/man1/

.PHONY: uninstall
uninstall:
	rm -f "$(DESTDIR)$(BIN_DIR)"/zvtov
	rm -f "$(DESTDIR)$(MAN_DIR)"/man1/zvtov.1

.SUFFIXES: .ml .mli .cmo .cmi .cmx .v .vo

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

.PHONY: doc odoc docdir
doc odoc docdir:
	@echo "The documentation for zvtov is not yet available :(" >&2

.PHONY: clean
clean:
	rm -f .#*
	rm -f *.cm* *.o *.annot *.output
	rm -f lexer_coq.ml parser_coq.ml
	rm -f parser_coq.mli
	rm -f zvtov *.bin *.byt

.PHONY: unconfigure
unconfigure:
	rm -f ./.config_var

.PHONY: distclean
distclean: clean unconfigure

.PHONY: depend
depend: $(IMPL) $(INTF)
	$(CAMLDEP) $(IMPL) $(INTF) >.depend

include .depend
