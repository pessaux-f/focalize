# $Id: Makefile,v 1.8 2008-09-18 22:39:00 weis Exp $

ROOTDIR = ..

include $(ROOTDIR)/Makefile.config

CAML_COMP_FLAGS = -I ../basement -I ../parser -I ../typing\
	-I ../commoncodegen

BYTOBJS = species_record_type_generation.cmo rec_let_gen.cmo\
	species_coq_generation.cmo type_coq_generation.cmo\
	main_coq_generation.cmo

include $(ROOTDIR)/Makefile.common
