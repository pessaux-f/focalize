SPECIES = "let I = taquin\#Int in \
           let v = I!of_int(5) in \
					 let ts = taquin\#Taquin_square in \
					 let b = taquin\#Board(I, v, v, ts) in \
             taquin\#Taquin(v, ts, b)"
FOCTEST_OPTS=-funspec random
ADDITIONAL_LIBRARY=
LIBRARY = taquin
PROPERTIES = 'feasibility'

NUMBER_OF_TEST=100
SIZE_OF_ELEM=$(shell expr 100)
XMLFILE=test_report.xml

CMO_TO_LINK = caml_basics.cmo openmath.cmo basics0.cmo
							
OCAML_OBJ= unix.cma
XSLTPATH=../xml

include ../Makefile.test
