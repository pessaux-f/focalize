SPECIES="let p1 = parameterized_species\#A in \
         let p2 = parameterized_species\#B(p1) in \
				 let p3 = parameterized_species\#C(p1,p2) in \
				   parameterized_species\#D(p1,p2,p3)"
LIBRARY=parameterized_species
FOCTEST_OPTS=
PROPERTIES='equal_commutes'
NUMBER_OF_TEST=100
SIZE_OF_ELEM=10
XMLFILE=prm_equal_comutes.xml

CMO_TO_LINK = ml_builtins.cmo sets_externals.cmo basics.cmo  sets.cmo

OCAML_OBJ= unix.cma
XSLTPATH=../xml

include ../Makefile.test
