require('dedukti')
dk_dk_builtins = { }
require("cc")
require("dk_tuple")
require("dk_opt")
require("dk_bool")
dk_dk_builtins.Object_c = { cid = "dk_builtins.Object" ; args = { } }
dk_dk_builtins.Object_t = { tbox_cty = { ctype=true } }
check_ext(dk_cc,'[ Lua ]  cc is undefined.')
dk_dk_builtins.collection_c = { cid = "dk_builtins.collection" ; args = { } }
dk_dk_builtins.collection_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
check_ext(dk_dk_builtins,'[ Lua ]  dk_builtins is undefined.')
dk_dk_builtins.Collection_c = app( app0(dk_cc.eT_c) , app0(dk_dk_builtins.collection_c) )
dk_dk_builtins.Collection_t = { tapp_f = dk_cc.eT_t ; tapp_a = dk_dk_builtins.collection_t ; tapp_ca = { clazy = function() return app0(dk_dk_builtins.collection_c) end } }
dk_dk_builtins.collmeth__rep_c = { cid = "dk_builtins.collmeth__rep" ; args = { } }
dk_dk_builtins.collmeth__rep_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_builtins.Collection_c) end } ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } }
dk_dk_builtins.unknown_type_c = { cid = "dk_builtins.unknown_type" ; args = { } }
dk_dk_builtins.unknown_type_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
dk_dk_builtins.unknown_def_c = { cid = "dk_builtins.unknown_def" ; args = { } }
dk_dk_builtins.unknown_def_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app0(dk_dk_builtins.unknown_type_c) ) end } }
dk_dk_builtins.unknown_proof_c = { cid = "dk_builtins.unknown_proof" ; args = { } }
dk_dk_builtins.unknown_proof_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app0(dk_dk_builtins.unknown_type_c) ) end } }
dk_dk_builtins.string_c = { cid = "dk_builtins.string" ; args = { } }
dk_dk_builtins.string_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
dk_dk_builtins.some_string_c = { cid = "dk_builtins.some_string" ; args = { } }
dk_dk_builtins.some_string_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app0(dk_dk_builtins.string_c) ) end } }
require("dk_nat")
require("dk_int")
require("dk_binary_nat")
require("dk_machine_int")
require("dk_logic")
check_ext(dk_dk_logic,'[ Lua ]  dk_logic is undefined.')
dk_dk_builtins.prop_c = app0(dk_dk_logic.Prop_c)
dk_dk_builtins.prop_t = dk_dk_logic.Prop_t
