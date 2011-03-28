SPECIES = 'let a = extern_var\#A in extern_var\#B(a)'
ADDITIONAL_LIBRARY=
LIBRARY = extern_var
PROPERTIES = 'list_length_correct'

NUMBER_OF_TEST=10
SIZE_OF_ELEM=$(shell expr 10)
XMLFILE=test_report.xml

CMO_TO_LINK = ml_builtins.cmo sets_externals.cmo basics.cmo  sets.cmo
							
OCAML_OBJ= unix.cma nums.cma
XSLTPATH=../xml

include ../Makefile.test
