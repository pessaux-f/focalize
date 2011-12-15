SPECIES = ' \
let zero = my_progs\#List_0 in \
let un     = my_progs\#List_1(zero) in \
let deux   = my_progs\#List_2(zero,un) in \
let trois  = my_progs\#List_3(zero,un,deux) in \
let quatre = my_progs\#List_4(zero,un,deux,trois) in \
let cinq   = my_progs\#List_5(zero,un,deux,trois,quatre) in \
let six    = my_progs\#List_6(zero,un,deux,trois,quatre,cinq) in \
let sept   = my_progs\#List_7(zero,un,deux,trois,quatre,cinq,six) in \
let huit   = my_progs\#List_8(zero,un,deux,trois,quatre,cinq,six,sept) in \
let neuf   = my_progs\#List_9(zero,un,deux,trois,quatre,cinq,six,sept,huit) in \
my_progs\#List_10(zero,un,deux,trois,quatre,cinq,six,sept,huit,neuf) ' 

# 'list_4(list_0,list_1,list_2,list_3)'
LIBRARY = my_progs
PROPERTIES =  'always_well_formed, \
               same_elem, \
							 sort_correct, \
               insert_two_first, \
							 insert_correct, \
               cons_hd, \
               cons_hd2, \
               plus_sort_stable, \
               plus_dual_minus, \
							 commute_minus_is_opposite, \
							 mult_minus_one_revs_sort '
NUMBER_OF_TEST=100
SIZE_OF_ELEM=$(shell expr 1073741823 / 2)
XMLFILE=test_report.xml

CMO_TO_LINK = ml_builtins.cmo sets_externals.cmo basics.cmo sets.cmo
#sets_orders_externals.cmo sets_orders.cmo sets.cmo

OCAML_OBJ= unix.cma nums.cma
XSLTPATH=../xml

include ../Makefile.test
