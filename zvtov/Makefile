#  Copyright 2004 INRIA
#  $Id: Makefile,v 1.6 2004-10-29 08:42:29 doligez Exp $

CAMLFLAGS = -warn-error A -g

CAMLC = ocamlc
CAMLCFLAGS = ${CAMLFLAGS} ${DEBUGFLAGS}

CAMLLEX = ocamllex
CAMLYACC = ocamlyacc

MODULES = cache invoke token lexer parser main

IMPL = ${MODULES:%=%.ml}
INTF = ${MODULES:%=%.mli}
OBJBYT = ${MODULES:%=%.cmo}

include .config_var

.PHONY: default
default: all

zvtov: ${OBJBYT}
	${CAMLC} ${CAMLCFLAGS} -o zvtov ${OBJBYT}

.PHONY: all
all: zvtov

.PHONY: install
install:
	mkdir -p ${BINDIR}
	cp zvtov ${BINDIR}/

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	${CAMLC} ${CAMLCFLAGS} -c $*.ml

.mli.cmi:
	${CAMLC} ${CAMLCFLAGS} -c $*.mli

lexer.ml: lexer.mll
	${CAMLLEX} lexer.mll

.PHONY: clean
clean:
	rm -f *.cmo *.cmi
	rm -f lexer.ml
	rm -f Makefile.bak zvtov

.PHONY: depend
depend: ${IMPL} ${INTF}
	mv Makefile Makefile.bak
	(sed -e '/^#DEPENDENCIES/q' Makefile.bak; \
         ocamldep ${IMPL} ${INTF}) \
	>Makefile

# Do NOT change anything below this line.
#DEPENDENCIES
cache.cmo: cache.cmi 
cache.cmx: cache.cmi 
invoke.cmo: cache.cmi invoke.cmi 
invoke.cmx: cache.cmx invoke.cmi 
token.cmo: token.cmi 
token.cmx: token.cmi 
lexer.cmo: token.cmi lexer.cmi 
lexer.cmx: token.cmx lexer.cmi 
parser.cmo: invoke.cmi lexer.cmi token.cmi parser.cmi 
parser.cmx: invoke.cmx lexer.cmx token.cmx parser.cmi 
main.cmo: cache.cmi invoke.cmi parser.cmi main.cmi 
main.cmx: cache.cmx invoke.cmx parser.cmx main.cmi 
lexer.cmi: token.cmi 
