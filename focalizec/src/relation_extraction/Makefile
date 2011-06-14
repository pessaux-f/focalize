ROOT_DIR=../..

include $(ROOT_DIR)/Makefile.config

CAML_COMP_FLAGS=-w A -dtypes

CAML_INCLUDES=\
 -I ../basement -I ../parser -I ../typing\
 -I ../commoncodegen

BYT_OBJS=\
 pred.cmo foc2spec.cmo focgen.cmo rel_ext.cmo

include $(ROOT_DIR)/Makefile.common

depend-local:
	$(CAML_DEP) $(BYT_OBJS:.cmo=.mli) $(BYT_OBJS:.cmo=.ml) > .depend

clean-local:
