SPECIES = " \
  let a = model\#S_rho_blp in \
	let b = model\#S_subjects in \
	let c = model\#S_objects in \
	let d = model\#S_access_mode in \
	let e = model\#S_access(b, c, d) in \
	let f = model\#S_set_of(e) in \
	let g = model\#S_get_release in \
	let h = model\#S_requests_gal(b, c, d, g) in \
	let i = model\#S_decisions in \
		model\#S_models_blp(a, b, c, d, e, f, g, h, i)"

LIBRARY = model

PROPERTIES =  'prop2_false_omega'

NUMBER_OF_TEST=1
#SIZE_OF_ELEM=$(shell expr 10)
SIZE_OF_ELEM=$(shell expr 3)
XMLFILE=test_report_fichier.xml

FOCTEST_OPTS=-sicstus -sicstus-path \
             ~/travail/cvs/focalize/focalizec/src/focalizetest/files_others/sicstus \
             -sicstus-opt meta \

#-sicstus-globalstk 256 \
             -sicstus-localstk 256 \
						 -sicstus-trailstk 256 \
             -sicstus-choicestk 256

CMO_TO_LINK = ml_builtins.cmo sets_externals.cmo basics.cmo sets_orders_externals.cmo sets_orders.cmo sets.cmo

OCAML_OBJ= unix.cma nums.cma
XSLTPATH=../xml

include ../Makefile.test
