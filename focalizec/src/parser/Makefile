# $Id: Makefile,v 1.21 2007-07-12 11:04:46 pessaux Exp $

RM = rm -rf

CAMLBYT = ocamlc -warn-error A -g
CAMLBIN = ocamlopt -warn-error A

CAMLYAC = ocamlyacc -v
CAMLLEX = ocamllex

CAML_COMP_FLAGS = -I ../../focc/

BYTOBJS = configuration.cmo lexer.cmo parser.cmo types.cmo parse_file.cmo \
	handy.cmo parsetree_utils.cmo dump_ptree.cmo sourcify.cmo \
	oldsourcify.cmo files.cmo env.cmo infer.cmo check_file.cmo

# Uncomment to use Pierre's pretty printer instead.
#BYTOBJS = configuration.cmo \
#	lexer.cmo parser.cmo parse_file.cmo \
#        lib_pp.cmo printtree.cmo printer.cmo \
#	../../focc/loc.cmo new2old.cmo \
#        check_file.cmo


all:: $(BYTOBJS)
	$(CAMLBYT) -o check_file $(BYTOBJS)

clean::
	$(RM) *~ *.cm* log lexer.ml lexer.mli
	$(RM) parser.ml parser.mli parser.conflicts parser.output

depend:
	ocamldep $(CAML_COMP_FLAGS) *.mli *.ml > ./.depend

explain:
	(menhir --explain parser.mly 2>&1) > menhir.out

# Uncomment to use Pierre's pretty printer instead.
# printer.ml: parsetree.mli
#	~weis/Work/Db/camlpp -keep -f parsetree.mli -t printer.ml

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
