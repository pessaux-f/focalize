require('dedukti')
dk_cc = { }
dk_cc.uT_c = { cid = "cc.uT" ; args = { } }
dk_cc.uT_t = { tbox_cty = { ctype=true } }
check_ext(dk_cc,'[ Lua ]  cc is undefined.')
dk_cc.eT_c = { cid = "cc.eT" ; args = { } }
dk_cc.eT_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (dummy) return { ctype=true } end } }
dk_cc.uuT_c = { cid = "cc.uuT" ; args = { } }
dk_cc.uuT_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
dk_cc.e_c = { cid = "cc.e" ; args = { } }
dk_cc.e_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (dummy) return { ctype=true } end } end } }
dk_cc.ee_c = { cid = "cc.ee" ; args = { } }
dk_cc.ee_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } end } }
dk_cc.Pi_TTT_c = { cid = "cc.Pi_TTT" ; args = { } }
dk_cc.Pi_TTT_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (X_c) return { cpi_cty = { cpi_cty = app( app0(dk_cc.eT_c) , X_c ) ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } end } }
dk_cc.Pi_TAA_c = { cid = "cc.Pi_TAA" ; args = { } }
dk_cc.Pi_TAA_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = app0(dk_cc.uT_c) ; cpi_f = function (X_c) return { cpi_cty = { cpi_cty = app( app0(dk_cc.eT_c) , X_c ) ; cpi_f = function (dummy) return app( app0(dk_cc.eT_c) , A_c ) end } ; cpi_f = function (dummy) return app( app0(dk_cc.eT_c) , A_c ) end } end } end } }
dk_cc.Arrow_c = { clam_f = function (t1_c) return { clam_f = function (t2_c) return app( app( app0(dk_cc.Pi_TTT_c) , t1_c ) , { clam_f = function (x_c) return t2_c end } ) end } end }
dk_cc.Arrow_t = { tlam_tty = dk_cc.uT_t ; tlam_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; tlam_f =  function (t1_t, t1_c) return { tlam_tty = dk_cc.uT_t ; tlam_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; tlam_f =  function (t2_t, t2_c) return { tapp_f = { tapp_f = dk_cc.Pi_TTT_t ; tapp_a = t1_t ; tapp_ca = t1_c } ; tapp_a = { tlam_tty = { tapp_f = dk_cc.eT_t ; tapp_a = t1_t ; tapp_ca = t1_c } ; tlam_cty = { clazy = function() return app( app0(dk_cc.eT_c) , t1_c ) end } ; tlam_f =  function (x_t, x_c) return t2_t end } ; tapp_ca = { clam_f = function (x_c) return t2_c end } } end } end }
dk_cc.eT_c = { cid="cc.eT" ; arity = 1 ; args = { } ; f = function(y1)
local y1 = force2(y1)
if y1.cid == "cc.Pi_TTT" then
local X_c, Y_c = y1.args[1], y1.args[2]
return { cpi_cty = app( app0(dk_cc.eT_c) , X_c ) ; cpi_f = function (x_c) return app( app0(dk_cc.eT_c) , app( Y_c , x_c ) ) end }
elseif y1.cid == "cc.uuT" then
return app0(dk_cc.uT_c)
elseif y1.cid == "cc.ee" then
local A_c, a_c = y1.args[1], y1.args[2]
return app( app( app0(dk_cc.e_c) , A_c ) , a_c )
else
return nil
end
end }
dk_cc.e_c = { cid="cc.e" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y2.cid == "cc.Pi_TAA" then
local dummy, X_c, Y_c, A_c = y2.args[1], y2.args[2], y2.args[3], y1
return { cpi_cty = app( app0(dk_cc.eT_c) , X_c ) ; cpi_f = function (x_c) return app( app( app0(dk_cc.e_c) , A_c ) , app( Y_c , x_c ) ) end }
else
return nil
end
end }
