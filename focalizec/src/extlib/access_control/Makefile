# Makefile for access control models

FOCC=../../toplevel/./check_file

CAMLC=ocamlc

ZENONC=zvtov

COQC=coqc

WARN=-w a

all: access_control.fo hru.fo rbac.fo ticket.fo unix.fo

clean:
	rm -f *.fo *.ml *.mli *.cmi *.cmo *.zv *.v *.pfc

.PRECIOUS: *.mli *.fo

.SUFFIXES: .foc .fo .ml .mli .cmi .cmo .zv .v .vo

.foc.fo:
	$(FOCC) $<

.ml.cmo:
	if test -e $*.mli; then make $*.cmi; fi
	$(CAMLC) $(WARN) -c $<

.mli.cmi:
	$(CAMLC) $(WARN) -c $<

.zv.v:
	$(ZENONC) $<

.v.vo:
	$(COQC) $<
