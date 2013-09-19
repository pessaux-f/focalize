require('dedukti')
dk_dk_logic = { }
require("cc")
require("dk_bool")
check_ext(dk_cc,'[ Lua ]  cc is undefined.')
dk_dk_logic.Prop_c = { cid = "dk_logic.Prop" ; args = { } }
dk_dk_logic.Prop_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
check_ext(dk_dk_logic,'[ Lua ]  dk_logic is undefined.')
dk_dk_logic.P_c = app( app0(dk_cc.eT_c) , app0(dk_dk_logic.Prop_c) )
dk_dk_logic.P_t = { tapp_f = dk_cc.eT_t ; tapp_a = dk_dk_logic.Prop_t ; tapp_ca = { clazy = function() return app0(dk_dk_logic.Prop_c) end } }
dk_dk_logic.eP_c = app( app0(dk_cc.e_c) , app0(dk_dk_logic.Prop_c) )
dk_dk_logic.eP_t = { tapp_f = dk_cc.e_t ; tapp_a = dk_dk_logic.Prop_t ; tapp_ca = { clazy = function() return app0(dk_dk_logic.Prop_c) end } }
dk_dk_logic.eeP_c = app( app0(dk_cc.ee_c) , app0(dk_dk_logic.Prop_c) )
dk_dk_logic.eeP_t = { tapp_f = dk_cc.ee_t ; tapp_a = dk_dk_logic.Prop_t ; tapp_ca = { clazy = function() return app0(dk_dk_logic.Prop_c) end } }
check_ext(dk_dk_bool,'[ Lua ]  dk_bool is undefined.')
dk_dk_logic.ebP_c = { cid = "dk_logic.ebP" ; args = { } }
dk_dk_logic.ebP_t = { tbox_cty = { cpi_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app0(dk_dk_bool.bool_c) ) end } ; cpi_f = function (dummy) return app0(dk_dk_logic.P_c) end } }
dk_dk_logic.True_c = { cid = "dk_logic.True" ; args = { } }
dk_dk_logic.True_t = { tbox_cty = { clazy = function() return app0(dk_dk_logic.P_c) end } }
dk_dk_logic.False_c = { cid = "dk_logic.False" ; args = { } }
dk_dk_logic.False_t = { tbox_cty = { clazy = function() return app0(dk_dk_logic.P_c) end } }
dk_dk_logic.not_c = { cid = "dk_logic.not" ; args = { } }
dk_dk_logic.not_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_logic.P_c) end } ; cpi_f = function (dummy) return app0(dk_dk_logic.P_c) end } }
dk_dk_logic.and_c = { cid = "dk_logic.and" ; args = { } }
dk_dk_logic.and_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_logic.P_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_logic.P_c) ; cpi_f = function (dummy) return app0(dk_dk_logic.P_c) end } end } }
dk_dk_logic.or_c = { cid = "dk_logic.or" ; args = { } }
dk_dk_logic.or_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_logic.P_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_logic.P_c) ; cpi_f = function (dummy) return app0(dk_dk_logic.P_c) end } end } }
dk_dk_logic.xor_c = { cid = "dk_logic.xor" ; args = { } }
dk_dk_logic.xor_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_logic.P_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_logic.P_c) ; cpi_f = function (dummy) return app0(dk_dk_logic.P_c) end } end } }
dk_dk_logic.imp_c = { cid = "dk_logic.imp" ; args = { } }
dk_dk_logic.imp_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_logic.P_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_logic.P_c) ; cpi_f = function (dummy) return app0(dk_dk_logic.P_c) end } end } }
dk_dk_logic.eqv_c = { cid = "dk_logic.eqv" ; args = { } }
dk_dk_logic.eqv_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_logic.P_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_logic.P_c) ; cpi_f = function (dummy) return app0(dk_dk_logic.P_c) end } end } }
dk_dk_logic.forall_c = { cid = "dk_logic.forall" ; args = { } }
dk_dk_logic.forall_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (x_c) return app0(dk_dk_logic.P_c) end } ; cpi_f = function (dummy) return app0(dk_dk_logic.P_c) end } end } }
dk_dk_logic.exists_c = { cid = "dk_logic.exists" ; args = { } }
dk_dk_logic.exists_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (x_c) return app0(dk_dk_logic.P_c) end } ; cpi_f = function (dummy) return app0(dk_dk_logic.P_c) end } end } }
dk_dk_logic.I_c = { cid = "dk_logic.I" ; args = { } }
dk_dk_logic.I_t = { tbox_cty = { clazy = function() return app( app0(dk_dk_logic.eP_c) , app0(dk_dk_logic.True_c) ) end } }
dk_dk_logic.ebP_c = { cid="dk_logic.ebP" ; arity = 1 ; args = { } ; f = function(y1)
local y1 = force2(y1)
if y1.cid == "dk_bool.true" then
return app0(dk_dk_logic.True_c)
elseif y1.cid == "dk_bool.false" then
return app0(dk_dk_logic.False_c)
else
return nil
end
end }
dk_dk_logic.forall_c = { cid="dk_logic.forall" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local A_c, f_c = y1, y2
return app( app( app( app0(dk_cc.Pi_TAA_c) , app0(dk_dk_logic.Prop_c) ) , A_c ) , f_c )
end }
dk_dk_logic.imp_c = { cid="dk_logic.imp" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local f1_c, f2_c = y1, y2
return app( app( app( app0(dk_cc.Pi_TAA_c) , app0(dk_dk_logic.Prop_c) ) , app( app0(dk_dk_logic.eeP_c) , f1_c ) ) , { clam_f = function (x_c) return f2_c end } )
end }
dk_dk_logic.magic_proof_c = { cid = "dk_logic.magic_proof" ; args = { } }
dk_dk_logic.magic_proof_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_logic.P_c) end } ; cpi_f = function (p_c) return app( app0(dk_dk_logic.eP_c) , p_c ) end } }
dk_dk_logic.equal_c = { cid = "dk_logic.equal" ; args = { } }
dk_dk_logic.equal_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (x_c) return { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (y_c) return app0(dk_dk_logic.P_c) end } end } end } }
dk_dk_logic.refl_c = { cid = "dk_logic.refl" ; args = { } }
dk_dk_logic.refl_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (x_c) return app( app0(dk_dk_logic.eP_c) , app( app( app( app0(dk_dk_logic.equal_c) , A_c ) , x_c ) , x_c ) ) end } end } }
dk_dk_logic.equal_ind_c = { cid = "dk_logic.equal_ind" ; args = { } }
dk_dk_logic.equal_ind_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } ; cpi_f = function (P_c) return { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (x_c) return { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (y_c) return { cpi_cty = app( app0(dk_dk_logic.eP_c) , app( app( app( app0(dk_dk_logic.equal_c) , A_c ) , x_c ) , y_c ) ) ; cpi_f = function (dummy) return { cpi_cty = app( app0(dk_cc.eT_c) , app( P_c , x_c ) ) ; cpi_f = function (dummy) return app( app0(dk_cc.eT_c) , app( P_c , y_c ) ) end } end } end } end } end } end } }
dk_dk_logic.equal_congr_c = { clam_f = function (A_c) return { clam_f = function (B_c) return { clam_f = function (f_c) return { clam_f = function (x_c) return { clam_f = function (y_c) return { clam_f = function (H_c) return app( app( app( app( app( app( app0(dk_dk_logic.equal_ind_c) , A_c ) , { clam_f = function (z_c) return app( app0(dk_dk_logic.eeP_c) , app( app( app( app0(dk_dk_logic.equal_c) , B_c ) , app( f_c , x_c ) ) , app( f_c , z_c ) ) ) end } ) , x_c ) , y_c ) , H_c ) , app( app( app0(dk_dk_logic.refl_c) , B_c ) , app( f_c , x_c ) ) ) end } end } end } end } end } end }
dk_dk_logic.equal_congr_t = { tlam_tty = dk_cc.uT_t ; tlam_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; tlam_f =  function (A_t, A_c) return { tlam_tty = dk_cc.uT_t ; tlam_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; tlam_f =  function (B_t, B_c) return { tlam_tty = { tpi_tty = { tapp_f = dk_cc.eT_t ; tapp_a = A_t ; tapp_ca = A_c } ; tpi_cty = { clazy = function() return app( app0(dk_cc.eT_c) , A_c ) end } ; tpi_f = function (dummy1,dummy2) return { tapp_f = dk_cc.eT_t ; tapp_a = B_t ; tapp_ca = B_c } end } ; tlam_cty = { cpi_cty = { clazy = function() return app( app0(dk_cc.eT_c) , A_c ) end } ; cpi_f = function (dummy) return app( app0(dk_cc.eT_c) , B_c ) end } ; tlam_f =  function (f_t, f_c) return { tlam_tty = { tapp_f = dk_cc.eT_t ; tapp_a = A_t ; tapp_ca = A_c } ; tlam_cty = { clazy = function() return app( app0(dk_cc.eT_c) , A_c ) end } ; tlam_f =  function (x_t, x_c) return { tlam_tty = { tapp_f = dk_cc.eT_t ; tapp_a = A_t ; tapp_ca = A_c } ; tlam_cty = { clazy = function() return app( app0(dk_cc.eT_c) , A_c ) end } ; tlam_f =  function (y_t, y_c) return { tlam_tty = { tapp_f = dk_dk_logic.eP_t ; tapp_a = { tapp_f = { tapp_f = { tapp_f = dk_dk_logic.equal_t ; tapp_a = A_t ; tapp_ca = A_c } ; tapp_a = x_t ; tapp_ca = x_c } ; tapp_a = y_t ; tapp_ca = y_c } ; tapp_ca = { clazy = function() return app( app( app( app0(dk_dk_logic.equal_c) , A_c ) , x_c ) , y_c ) end } } ; tlam_cty = { clazy = function() return app( app0(dk_dk_logic.eP_c) , app( app( app( app0(dk_dk_logic.equal_c) , A_c ) , x_c ) , y_c ) ) end } ; tlam_f =  function (H_t, H_c) return { tapp_f = { tapp_f = { tapp_f = { tapp_f = { tapp_f = { tapp_f = dk_dk_logic.equal_ind_t ; tapp_a = A_t ; tapp_ca = A_c } ; tapp_a = { tlam_tty = { tapp_f = dk_cc.eT_t ; tapp_a = A_t ; tapp_ca = A_c } ; tlam_cty = { clazy = function() return app( app0(dk_cc.eT_c) , A_c ) end } ; tlam_f =  function (z_t, z_c) return { tapp_f = dk_dk_logic.eeP_t ; tapp_a = { tapp_f = { tapp_f = { tapp_f = dk_dk_logic.equal_t ; tapp_a = B_t ; tapp_ca = B_c } ; tapp_a = { tapp_f = f_t ; tapp_a = x_t ; tapp_ca = x_c } ; tapp_ca = { clazy = function() return app( f_c , x_c ) end } } ; tapp_a = { tapp_f = f_t ; tapp_a = z_t ; tapp_ca = z_c } ; tapp_ca = { clazy = function() return app( f_c , z_c ) end } } ; tapp_ca = { clazy = function() return app( app( app( app0(dk_dk_logic.equal_c) , B_c ) , app( f_c , x_c ) ) , app( f_c , z_c ) ) end } } end } ; tapp_ca = { clam_f = function (z_c) return app( app0(dk_dk_logic.eeP_c) , app( app( app( app0(dk_dk_logic.equal_c) , B_c ) , app( f_c , x_c ) ) , app( f_c , z_c ) ) ) end } } ; tapp_a = x_t ; tapp_ca = x_c } ; tapp_a = y_t ; tapp_ca = y_c } ; tapp_a = H_t ; tapp_ca = H_c } ; tapp_a = { tapp_f = { tapp_f = dk_dk_logic.refl_t ; tapp_a = B_t ; tapp_ca = B_c } ; tapp_a = { tapp_f = f_t ; tapp_a = x_t ; tapp_ca = x_c } ; tapp_ca = { clazy = function() return app( f_c , x_c ) end } } ; tapp_ca = { clazy = function() return app( app( app0(dk_dk_logic.refl_c) , B_c ) , app( f_c , x_c ) ) end } } end } end } end } end } end } end }
