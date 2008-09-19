# $Id: Makefile,v 1.10 2008-09-18 22:39:00 weis Exp $

ROOTDIR = ..

include $(ROOTDIR)/Makefile.config

CAML_COMP_FLAGS = -I ../basement -I ../parser

BYTOBJS = env.cmo scoping.cmo substExpr.cmo substColl.cmo ast_equal.cmo \
	dep_analysis.cmo param_dep_analysis.cmo infer.cmo

include $(ROOTDIR)/Makefile.common
