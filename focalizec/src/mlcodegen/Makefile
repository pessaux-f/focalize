# $Id: Makefile,v 1.7 2008-09-18 22:39:00 weis Exp $

ROOTDIR = ..

include $(ROOTDIR)/Makefile.config

CAML_COMP_FLAGS = -I ../basement -I ../parser -I ../typing -I ../commoncodegen

BYTOBJS = misc_ml_generation.cmo type_ml_generation.cmo\
	base_exprs_ml_generation.cmo species_ml_generation.cmo\
	main_ml_generation.cmo

include $(ROOTDIR)/Makefile.common
