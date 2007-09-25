# $Id: Makefile,v 1.5 2007-09-25 15:29:10 pessaux Exp $

include ./Makefile.config
CAML_COMP_FLAGS = -I ../../focc/

BYTOBJS = configuration.cmo lexer.cmo parser.cmo handy.cmo  types.cmo \
	parse_file.cmo parsetree_utils.cmo dump_ptree.cmo sourcify.cmo \
	oldsourcify.cmo files.cmo env.cmo infer.cmo check_file.cmo



all::
	$(CD) basement ; $(MAKE) all
	$(CD) parser ; $(MAKE) all
	$(CD) typing ; $(MAKE) all
	$(CD) mlcodegen ; $(MAKE) all
	$(CD) toplevel ; $(MAKE) all
	$(CD) lib ; $(MAKE) all

clean::
	$(RM) *~
	$(CD) basement ; $(MAKE) clean
	$(CD) parser ; $(MAKE) clean
	$(CD) typing ; $(MAKE) clean
	$(CD) mlcodegen ; $(MAKE) clean
	$(CD) toplevel ; $(MAKE) clean
	$(CD) lib ; $(MAKE) clean

doc:: 
	$(CD) basement ; $(MAKE) doc
	$(CD) parser ; $(MAKE) doc
	$(CD) typing ; $(MAKE) doc
	$(CD) mlcodegen ; $(MAKE) doc
	$(CD) toplevel ; $(MAKE) doc
	$(CD) lib ; $(MAKE) doc

depend:
	$(CD) basement ; $(MAKE) depend
	$(CD) parser ; $(MAKE) depend
	$(CD) typing ; $(MAKE) depend
	$(CD) mlcodegen ; $(MAKE) depend
	$(CD) toplevel ; $(MAKE) depend
	$(CD) lib ; $(MAKE) depend



.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cmx 

.ml.cmo:
	$(CAMLBYT) $(CAML_COMP_FLAGS) -c $<

.mli.cmi:
	$(CAMLBYT) $(CAML_COMP_FLAGS) -c $<

.ml.cmx:
	$(CAMLBIN) $(CAML_COMP_FLAGS) -c $<

.mly.ml:
	$(CAMLYACC) $<

.mll.ml:
	$(CAMLLEX) $<


include .depend
