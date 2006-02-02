#  Copyright 2004 INRIA
#  $Id: Makefile,v 1.11 2006-02-02 13:30:03 doligez Exp $

CAMLFLAGS = -warn-error A

CAMLC = ocamlc
CAMLCFLAGS = ${CAMLFLAGS} ${DEBUGFLAGS} ${XMLRPCFLAGS}

CAMLLEX = ocamllex
CAMLYACC = ocamlyacc

MODULES = version misc \
	  cache invoke options ${XMLRPCFILE} token lexer parser main

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

.PHONY: clean
clean:
	rm -f *.cmo *.cmi *.annot
	rm -f lexer.ml
	rm -f zvtov

.PHONY: archclean
archclean: clean
	rm -f .config_var

.PHONY: depend
depend: ${IMPL} ${INTF}
	ocamldep ${IMPL} ${INTF} >.depend

include .depend
