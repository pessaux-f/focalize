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

# Reading configuration settings.
include ../.config_var


CAMLFLAGS = -warn-error $(WARN_ERROR)

# Variables CAMLBYT, CAMLBIN, CAMLLEX, CAMLYACC, CAMLDEP and CAMLDOC are
# defined at configuration time their value is recorded in ../.config_var.
CAMLBINFLAGS = $(CAMLFLAGS) $(BIN_DEBUG_FLAGS) $(XMLRPC_FLAGS)

CAMLBYTFLAGS = $(CAMLFLAGS) $(BYT_DEBUG_FLAGS) $(XMLRPC_FLAGS)

MODULES = version misc options cache \
          expr parser_coq lexer_coq parser_dk lexer_dk invoke invoke_cime \
          $(XMLRPC_MODULE) token lexer parser main

LEXERS = lexer_coq lexer_dk lexer
PARSERS = parser_coq parser_dk

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
	$(SUDO) mkdir -p $(INSTALL_BIN_DIR)
	$(SUDO) cp -p zvtov $(INSTALL_BIN_DIR)/
	$(SUDO) mkdir -p $(INSTALL_MAN_DIR)/man1
	$(SUDO) cp zvtov.1 $(INSTALL_MAN_DIR)/man1/

uninstall:
	$(SUDO) rm -f $(INSTALL_BIN_DIR)/zvtov
	$(SUDO) rm -f $(INSTALL_MAN_DIR)/man1/zvtov.1

.PHONY: uninstall

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

lexer_dk.ml: lexer_dk.mll
	$(CAMLLEX) lexer_dk.mll

parser_dk.ml: parser_dk.mly
	$(CAMLYACC) -v parser_dk.mly

parser_dk.mli: parser_dk.ml
	:

.PHONY: doc odoc docdir
doc odoc docdir:
	@echo "The documentation for zvtov is not yet available :(" >&2

.PHONY: clean
clean:
	rm -f .#*
	rm -f *.cm* *.o *.annot *.output
	rm -f lexer_coq.ml parser_coq.ml
	rm -f lexer_dk.ml parser_dk.ml
	rm -f parser_coq.mli
	rm -f parser_dk.mli
	rm -f zvtov *.bin *.byt

.PHONY: depend
depend: $(IMPL) $(INTF)
	$(CAMLDEP) $(IMPL) $(INTF) >.depend

include .depend
