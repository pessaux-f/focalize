#  Copyright 2004 INRIA
#  $Id: Makefile,v 1.9 2005-11-09 15:22:03 doligez Exp $

CAMLFLAGS = -warn-error A

CAMLC = ocamlc
CAMLCFLAGS = ${CAMLFLAGS} ${DEBUGFLAGS} ${XMLRPCFLAGS}

CAMLLEX = ocamllex
CAMLYACC = ocamlyacc

MODULES = version cache invoke options ${XMLRPCFILE} token lexer parser main

IMPL = ${MODULES:%=%.ml}
INTF = ${MODULES:%=%.mli}
OBJBYT = ${MODULES:%=%.cmo}

include .config_var

.PHONY: default
default: all

zvtov: ${OBJBYT}
	${CAMLC} ${CAMLCFLAGS} ${XMLRPCLINK} -o zvtov ${OBJBYT}

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
version.cmo: version.cmi 
version.cmx: version.cmi 
cache.cmo: cache.cmi 
cache.cmx: cache.cmi 
invoke.cmo: cache.cmi invoke.cmi 
invoke.cmx: cache.cmx invoke.cmi 
options.cmo: version.cmi invoke.cmi cache.cmi options.cmi 
options.cmx: version.cmx invoke.cmx cache.cmx options.cmi 
token.cmo: token.cmi 
token.cmx: token.cmi 
lexer.cmo: token.cmi invoke.cmi lexer.cmi 
lexer.cmx: token.cmx invoke.cmx lexer.cmi 
parser.cmo: token.cmi lexer.cmi invoke.cmi parser.cmi 
parser.cmx: token.cmx lexer.cmx invoke.cmx parser.cmi 
main.cmo: parser.cmi options.cmi invoke.cmi cache.cmi main.cmi 
main.cmx: parser.cmx options.cmx invoke.cmx cache.cmx main.cmi 
lexer.cmi: token.cmi 
