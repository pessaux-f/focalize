require('dedukti')
dk_basics = { }
require("dk_builtins")
check_ext(dk_cc,'[ Lua ]  cc is undefined.')
dk_basics.int_c = { cid = "basics.int" ; args = { } }
dk_basics.int_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
check_ext(dk_dk_int,'[ Lua ]  dk_int is undefined.')
dk_basics.int_c = { cid="basics.int" ; arity = 0 ; args = { } ; f = function() return app0(dk_dk_int.int_c) end }
dk_basics.unit_c = { cid = "basics.unit" ; args = { } }
dk_basics.unit_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
dk_basics.float_c = { cid = "basics.float" ; args = { } }
dk_basics.float_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
dk_basics.char_c = { cid = "basics.char" ; args = { } }
dk_basics.char_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
dk_basics.string_c = { cid = "basics.string" ; args = { } }
dk_basics.string_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
check_ext(dk_dk_builtins,'[ Lua ]  dk_builtins is undefined.')
dk_basics.string_c = { cid="basics.string" ; arity = 0 ; args = { } ; f = function() return app0(dk_dk_builtins.string_c) end }
dk_basics.bool_c = { cid = "basics.bool" ; args = { } }
dk_basics.bool_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
check_ext(dk_dk_bool,'[ Lua ]  dk_bool is undefined.')
dk_basics.bool_c = { cid="basics.bool" ; arity = 0 ; args = { } ; f = function() return app0(dk_dk_bool.bool_c) end }
dk_basics.list_c = { cid = "basics.list" ; args = { } }
dk_basics.list_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } }
check_ext(dk_dk_list,'[ Lua ]  dk_list is undefined.')
dk_basics.list_c = { cid="basics.list" ; arity = 0 ; args = { } ; f = function() return app0(dk_dk_list.list_c) end }
check_ext(dk_basics,'[ Lua ]  basics is undefined.')
dk_basics._amper__amper__c = { cid = "basics._amper__amper_" ; args = { } }
dk_basics._amper__amper__t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.bool_c) ) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.bool_c) ) , app0(dk_basics.bool_c) ) ) ) end } }
dk_basics._bar__bar__c = { cid = "basics._bar__bar_" ; args = { } }
dk_basics._bar__bar__t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.bool_c) ) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.bool_c) ) , app0(dk_basics.bool_c) ) ) ) end } }
dk_basics._tilda__tilda__c = { cid = "basics._tilda__tilda_" ; args = { } }
dk_basics._tilda__tilda__t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.bool_c) ) , app0(dk_basics.bool_c) ) ) end } }
dk_basics._bar__lt__gt__bar__c = { cid = "basics._bar__lt__gt__bar_" ; args = { } }
dk_basics._bar__lt__gt__bar__t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.bool_c) ) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.bool_c) ) , app0(dk_basics.bool_c) ) ) ) end } }
check_ext(dk_dk_tuple,'[ Lua ]  dk_tuple is undefined.')
dk_basics.pair_c = { cid = "basics.pair" ; args = { } }
dk_basics.pair_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (type_var__b_c) return { cpi_cty = app0(dk_cc.uT_c) ; cpi_f = function (type_var__a_c) return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , type_var__a_c ) , app( app( app0(dk_cc.Arrow_c) , type_var__b_c ) , app( app( app0(dk_dk_tuple.Tuple_c) , type_var__a_c ) , type_var__b_c ) ) ) ) end } end } }
dk_basics.fst_c = { cid = "basics.fst" ; args = { } }
dk_basics.fst_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (type_var__b_c) return { cpi_cty = app0(dk_cc.uT_c) ; cpi_f = function (type_var__a_c) return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app( app( app0(dk_dk_tuple.Tuple_c) , type_var__a_c ) , type_var__b_c ) ) , type_var__a_c ) ) end } end } }
dk_basics.snd_c = { cid = "basics.snd" ; args = { } }
dk_basics.snd_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (type_var__a_c) return { cpi_cty = app0(dk_cc.uT_c) ; cpi_f = function (type_var__b_c) return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app( app( app0(dk_dk_tuple.Tuple_c) , type_var__b_c ) , type_var__a_c ) ) , type_var__a_c ) ) end } end } }
dk_basics._hat__c = { cid = "basics._hat_" ; args = { } }
dk_basics._hat__t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.string_c) ) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.string_c) ) , app0(dk_basics.string_c) ) ) ) end } }
dk_basics._lt__hat__c = { cid = "basics._lt__hat_" ; args = { } }
dk_basics._lt__hat__t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.string_c) ) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.string_c) ) , app0(dk_basics.bool_c) ) ) ) end } }
dk_basics._equal_0x_c = { cid = "basics._equal_0x" ; args = { } }
dk_basics._equal_0x_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.bool_c) ) ) ) end } }
dk_basics._lt_0x_c = { cid = "basics._lt_0x" ; args = { } }
dk_basics._lt_0x_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.bool_c) ) ) ) end } }
dk_basics._lt__equal_0x_c = { cid = "basics._lt__equal_0x" ; args = { } }
dk_basics._lt__equal_0x_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.bool_c) ) ) ) end } }
dk_basics._gt__equal_0x_c = { cid = "basics._gt__equal_0x" ; args = { } }
dk_basics._gt__equal_0x_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.bool_c) ) ) ) end } }
dk_basics._gt_0x_c = { cid = "basics._gt_0x" ; args = { } }
dk_basics._gt_0x_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.bool_c) ) ) ) end } }
dk_basics._plus__c = { cid = "basics._plus_" ; args = { } }
dk_basics._plus__t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.int_c) ) ) ) end } }
dk_basics._dash__c = { cid = "basics._dash_" ; args = { } }
dk_basics._dash__t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.int_c) ) ) ) end } }
dk_basics._tilda_0x_c = { cid = "basics._tilda_0x" ; args = { } }
dk_basics._tilda_0x_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.int_c) ) ) end } }
dk_basics._star__c = { cid = "basics._star_" ; args = { } }
dk_basics._star__t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.int_c) ) ) ) end } }
dk_basics._slash__c = { cid = "basics._slash_" ; args = { } }
dk_basics._slash__t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.int_c) ) ) ) end } }
dk_basics._percent__c = { cid = "basics._percent_" ; args = { } }
dk_basics._percent__t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.int_c) ) ) ) end } }
dk_basics.succ0x_c = { cid = "basics.succ0x" ; args = { } }
dk_basics.succ0x_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.int_c) ) ) end } }
dk_basics.pred0x_c = { cid = "basics.pred0x" ; args = { } }
dk_basics.pred0x_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.int_c) ) ) end } }
dk_basics.max0x_c = { cid = "basics.max0x" ; args = { } }
dk_basics.max0x_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.int_c) ) ) ) end } }
dk_basics.min0x_c = { cid = "basics.min0x" ; args = { } }
dk_basics.min0x_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.int_c) ) ) ) end } }
dk_basics.abs0x_c = { cid = "basics.abs0x" ; args = { } }
dk_basics.abs0x_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.int_c) ) ) end } }
dk_basics.string_of_int_c = { cid = "basics.string_of_int" ; args = { } }
dk_basics.string_of_int_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.string_c) ) ) end } }
dk_basics.int_of_string_c = { cid = "basics.int_of_string" ; args = { } }
dk_basics.int_of_string_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.string_c) ) , app0(dk_basics.int_c) ) ) end } }
dk_basics._equal__c = { cid = "basics._equal_" ; args = { } }
dk_basics._equal__t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (type_var__a_c) return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , type_var__a_c ) , app( app( app0(dk_cc.Arrow_c) , type_var__a_c ) , app0(dk_basics.bool_c) ) ) ) end } }
dk_basics.syntactic_equal_c = { cid = "basics.syntactic_equal" ; args = { } }
dk_basics.syntactic_equal_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (type_var__a_c) return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , type_var__a_c ) , app( app( app0(dk_cc.Arrow_c) , type_var__a_c ) , app0(dk_basics.bool_c) ) ) ) end } }
dk_basics.print_int_c = { cid = "basics.print_int" ; args = { } }
dk_basics.print_int_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.int_c) ) , app0(dk_basics.unit_c) ) ) end } }
dk_basics.print_newline_c = { cid = "basics.print_newline" ; args = { } }
dk_basics.print_newline_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.unit_c) ) , app0(dk_basics.unit_c) ) ) end } }
dk_basics.print_string_c = { cid = "basics.print_string" ; args = { } }
dk_basics.print_string_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.string_c) ) , app0(dk_basics.unit_c) ) ) end } }
dk_basics.focalize_error_c = { cid = "basics.focalize_error" ; args = { } }
dk_basics.focalize_error_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (type_var__a_c) return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.string_c) ) , type_var__a_c ) ) end } }
dk_basics.species_instance__Basic_object_c = { cid = "basics.species_instance__Basic_object" ; args = { } }
dk_basics.species_instance__Basic_object_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
dk_basics.species_instance__Basic_object_c = { cid="basics.species_instance__Basic_object" ; arity = 0 ; args = { } ; f = function() return app0(dk_dk_builtins.collection_c) end }
dk_basics.methtype__Basic_object__parse_c = { cid = "basics.methtype__Basic_object__parse" ; args = { } }
dk_basics.methtype__Basic_object__parse_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
dk_basics.methtype__Basic_object__parse_c = { cid="basics.methtype__Basic_object__parse" ; arity = 0 ; args = { } ; f = function() return app( app( app0(dk_cc.Pi_TTT_c) , app0(dk_cc.uuT_c) ) , { clam_f = function (rep_c) return app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.string_c) ) , rep_c ) end } ) end }
dk_basics.meth__Basic_object__parse_c = { cid = "basics.meth__Basic_object__parse" ; args = { } }
dk_basics.meth__Basic_object__parse_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app0(dk_basics.methtype__Basic_object__parse_c) ) end } }
dk_basics.methtype__Basic_object__print_c = { cid = "basics.methtype__Basic_object__print" ; args = { } }
dk_basics.methtype__Basic_object__print_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
dk_basics.methtype__Basic_object__print_c = { cid="basics.methtype__Basic_object__print" ; arity = 0 ; args = { } ; f = function() return app( app( app0(dk_cc.Pi_TTT_c) , app0(dk_cc.uuT_c) ) , { clam_f = function (rep_c) return app( app( app0(dk_cc.Arrow_c) , rep_c ) , app0(dk_basics.string_c) ) end } ) end }
dk_basics.meth__Basic_object__print_c = { cid = "basics.meth__Basic_object__print" ; args = { } }
dk_basics.meth__Basic_object__print_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app0(dk_basics.methtype__Basic_object__print_c) ) end } }
dk_basics.collmeth__rep_c = { cid = "basics.collmeth__rep" ; args = { } }
dk_basics.collmeth__rep_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_builtins.Collection_c) end } ; cpi_f = function (c_c) return app0(dk_cc.uT_c) end } }
dk_basics.collmeth__parse_c = { cid = "basics.collmeth__parse" ; args = { } }
dk_basics.collmeth__parse_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_builtins.Collection_c) end } ; cpi_f = function (c_c) return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app0(dk_basics.string_c) ) , app( app0(dk_dk_builtins.collmeth__rep_c) , c_c ) ) ) end } }
dk_basics.collmeth__print_c = { cid = "basics.collmeth__print" ; args = { } }
dk_basics.collmeth__print_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_builtins.Collection_c) end } ; cpi_f = function (c_c) return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app( app0(dk_dk_builtins.collmeth__rep_c) , c_c ) ) , app0(dk_basics.string_c) ) ) end } }
dk_basics.partiel_c = { cid = "basics.partiel" ; args = { } }
dk_basics.partiel_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } }
dk_basics.constr__Failed_c = { cid = "basics.constr__Failed" ; args = { } }
dk_basics.constr__Failed_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (type_var__a_c) return app( app0(dk_cc.eT_c) , app( app0(dk_basics.partiel_c) , type_var__a_c ) ) end } }
dk_basics.constr__Unfailed_c = { cid = "basics.constr__Unfailed" ; args = { } }
dk_basics.constr__Unfailed_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (type_var__a_c) return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , type_var__a_c ) , app( app0(dk_basics.partiel_c) , type_var__a_c ) ) ) end } }
dk_basics.is_failed_c = { cid = "basics.is_failed" ; args = { } }
dk_basics.is_failed_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (type_var__a_c) return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app( app0(dk_basics.partiel_c) , type_var__a_c ) ) , app0(dk_basics.bool_c) ) ) end } }
dk_basics.non_failed_c = { cid = "basics.non_failed" ; args = { } }
dk_basics.non_failed_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (type_var__a_c) return app( app0(dk_cc.eT_c) , app( app( app0(dk_cc.Arrow_c) , app( app0(dk_basics.partiel_c) , type_var__a_c ) ) , type_var__a_c ) ) end } }
