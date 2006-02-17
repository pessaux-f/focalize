#  Copyright 2004 INRIA
#  $Id: Makefile,v 1.12 2006-02-17 15:36:33 lk Exp $

CAMLFLAGS = -warn-error A

CAMLC = ocamlc
CAMLCFLAGS = ${CAMLFLAGS} ${DEBUGFLAGS} ${XMLRPCFLAGS}

CAMLLEX = ocamllex
CAMLYACC = ocamlyacc

MODULES = version misc \
	  cache invoke \
	  expr parser_coq lexer_coq parser_sat lexer_sat invoke_cime \
	  ${XMLRPCFILE} token lexer parser options main

IMPL = ${MODULES:%=%.ml}
INTF = ${MODULES:%=%.mli}
OBJBYT = ${MODULES:%=%.cmo}

include .config_var


.PHONY: all
all: zvtov


zvtov: ${OBJBYT}
	${CAMLC} ${CAMLCFLAGS} ${XMLRPCLINK} -o zvtov ${OBJBYT}

.PHONY: install
install:
	mkdir -p "${BINDIR}"
	cp zvtov "${BINDIR}"/

.PHONY: uninstall
uninstall:
	rm -f "${BINDIR}"/zvtov

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	${CAMLC} ${CAMLCFLAGS} -c $*.ml

.mli.cmi:
	${CAMLC} ${CAMLCFLAGS} -c $*.mli

lexer.ml: lexer.mll
	${CAMLLEX} lexer.mll

lexer_coq.ml: lexer_coq.mll
	${CAMLLEX} lexer_coq.mll

lexer_sat.ml: lexer_sat.mll
	${CAMLLEX} lexer_sat.mll





parser_coq.ml: parser_coq.mly
	${CAMLYACC} -v parser_coq.mly

parser_coq.mli: parser_coq.ml
	:

parser_sat.ml: parser_sat.mly
	${CAMLYACC} -v parser_sat.mly

parser_sat.mli: parser_sat.ml
	:




.PHONY: clean
clean:
	rm -f *.cmo *.cmi *.annot
	rm -f lexer_coq.ml lexer_sat.ml parser_coq.ml parser_sat.ml
	rm -f parser_coq.mli parser_sat.mli
	rm -f zvtov

.PHONY: archclean
archclean: clean
	rm -f .config_var

.PHONY: depend
depend: ${IMPL} ${INTF}
	ocamldep ${IMPL} ${INTF} >.depend

include .depend
