SPECIES = 'let sn = small_naturals in lexicographic_product_additive_monoid_print(sn,sn)'
LIBRARY = add_print
FOCTEST_OPTS=
#PROPERTIES =  'sup_absorbes_add, geq_absorbes_add, inf_absorbes_add, \
#               leq_absorbes_add, order_compatible_with_cancellation, \
#               order_compatible_with_plus, geq_refines_order_sup, \
#               leq_refines_order_inf, geq_transitive, total_order, \
#               leq_transitive, order_sup_is_supremum, order_sup_is_transitive, \
#               order_inf_is_infimum, order_inf_is_transitive'
PROPERTIES = 'order_compatible_with_cancellation'
NUMBER_OF_TEST=100
SIZE_OF_ELEM=$(shell expr 1073741823 / 2)
XMLFILE=test_report.xml

CMO_TO_LINK = caml_basics.cmo openmath.cmo basics0.cmo sets_orders.cmo \
							iterators.cmo multiplicative_law.cmo array_foc.cmo \
							additive_law.cmo random_foc.cmo weak_structures.cmo \
							rings_fields.cmo products_foc.cmo integers.cmo \
							small_integers.cmo 
							
OCAML_OBJ= unix.cma nums.cma
XSLTPATH=../xml

include ../Makefile.test
