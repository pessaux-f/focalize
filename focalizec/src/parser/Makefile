CAMLBYT = ocamlc -warn-error A -g
CAMLBIN = ocamlopt -warn-error A

CAMLYAC = ocamlyacc -v
CAMLLEX = ocamllex

CAML_COMP_FLAGS = -I ../../focc/

BYTOBJS = configuration.cmo \
	lexer.cmo parser.cmo parse_file.cmo \
	new2old.cmo check_file.cmo

#BYTOBJS=configuration.cmo \
#   lexer.cmo parser.cmo parse_file.cmo \
#   lib_pp.cmo printer.cmo \
#   check_file.cmo \


RM = rm -rf

all:: $(BYTOBJS)
	$(CAMLBYT) -o check_file $(BYTOBJS)

clean::
	$(RM) *~ *.cm* log lexer.ml lexer.mli
	$(RM) parser.ml parser.mli parser.conflicts parser.output

depend:
	ocamldep $(CAML_COMP_FLAGS) *.mli *.ml > ./.depend

explain:
	(menhir --explain parser.mly 2>&1) > menhir.out

printer.ml: parsetree.mli
	~weis/Work/Db/camlpp -keep -f parsetree.mli -t printer.ml

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cmx 

.ml.cmo:
	$(CAMLBYT) $(CAML_COMP_FLAGS) -c $<

.mli.cmi:
	$(CAMLBYT) $(CAML_COMP_FLAGS) -c $<

.ml.cmx:
	$(CAMLBIN) $(CAML_COMP_FLAGS) -c $<

.mly.ml:
	$(CAMLYAC) $<

.mll.ml:
	$(CAMLLEX) $<

lexer.cmo: parsetree.cmi parser.cmi

parser.mli: parser.ml

include .depend
