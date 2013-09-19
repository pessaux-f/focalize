require('dedukti')
dk_dk_bool = { }
require("cc")
check_ext(dk_cc,'[ Lua ]  cc is undefined.')
dk_dk_bool.bool_c = { cid = "dk_bool.bool" ; args = { } }
dk_dk_bool.bool_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
check_ext(dk_dk_bool,'[ Lua ]  dk_bool is undefined.')
dk_dk_bool.B_c = app( app0(dk_cc.eT_c) , app0(dk_dk_bool.bool_c) )
dk_dk_bool.B_t = { tapp_f = dk_cc.eT_t ; tapp_a = dk_dk_bool.bool_t ; tapp_ca = { clazy = function() return app0(dk_dk_bool.bool_c) end } }
dk_dk_bool.true_c = { cid = "dk_bool.true" ; args = { } }
dk_dk_bool.true_t = { tbox_cty = { clazy = function() return app0(dk_dk_bool.B_c) end } }
dk_dk_bool.false_c = { cid = "dk_bool.false" ; args = { } }
dk_dk_bool.false_t = { tbox_cty = { clazy = function() return app0(dk_dk_bool.B_c) end } }
dk_dk_bool.match_c = { cid = "dk_bool.match" ; args = { } }
dk_dk_bool.match_t = { tbox_cty = { cpi_cty = { cpi_cty = { clazy = function() return app0(dk_dk_bool.B_c) end } ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } ; cpi_f = function (P_c) return { cpi_cty = app( app0(dk_cc.eT_c) , app( P_c , app0(dk_dk_bool.true_c) ) ) ; cpi_f = function (dummy) return { cpi_cty = app( app0(dk_cc.eT_c) , app( P_c , app0(dk_dk_bool.false_c) ) ) ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_bool.B_c) ; cpi_f = function (b_c) return app( app0(dk_cc.eT_c) , app( P_c , b_c ) ) end } end } end } end } }
dk_dk_bool.match_c = { cid="dk_bool.match" ; arity = 4 ; args = { } ; f = function(y1, y2, y3, y4)
local y1 = force2(y1)
local y2 = force2(y2)
local y3 = force2(y3)
local y4 = force2(y4)
if y4.cid == "dk_bool.true" then
local P_c, Ht_c, Hf_c = y1, y2, y3
return Ht_c
elseif y4.cid == "dk_bool.false" then
local P_c, Ht_c, Hf_c = y1, y2, y3
return Hf_c
else
return nil
end
end }
dk_dk_bool.ite_c = { clam_f = function (A_c) return { clam_f = function (b_c) return { clam_f = function (x_c) return { clam_f = function (y_c) return app( app( app( app( app0(dk_dk_bool.match_c) , { clam_f = function (b_c) return A_c end } ) , x_c ) , y_c ) , b_c ) end } end } end } end }
dk_dk_bool.ite_t = { tlam_tty = dk_cc.uT_t ; tlam_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; tlam_f =  function (A_t, A_c) return { tlam_tty = dk_dk_bool.B_t ; tlam_cty = { clazy = function() return app0(dk_dk_bool.B_c) end } ; tlam_f =  function (b_t, b_c) return { tlam_tty = { tapp_f = dk_cc.eT_t ; tapp_a = A_t ; tapp_ca = A_c } ; tlam_cty = { clazy = function() return app( app0(dk_cc.eT_c) , A_c ) end } ; tlam_f =  function (x_t, x_c) return { tlam_tty = { tapp_f = dk_cc.eT_t ; tapp_a = A_t ; tapp_ca = A_c } ; tlam_cty = { clazy = function() return app( app0(dk_cc.eT_c) , A_c ) end } ; tlam_f =  function (y_t, y_c) return { tapp_f = { tapp_f = { tapp_f = { tapp_f = dk_dk_bool.match_t ; tapp_a = { tlam_tty = dk_dk_bool.B_t ; tlam_cty = { clazy = function() return app0(dk_dk_bool.B_c) end } ; tlam_f =  function (b_t, b_c) return A_t end } ; tapp_ca = { clam_f = function (b_c) return A_c end } } ; tapp_a = x_t ; tapp_ca = x_c } ; tapp_a = y_t ; tapp_ca = y_c } ; tapp_a = b_t ; tapp_ca = b_c } end } end } end } end }
dk_dk_bool.iteb_c = app( app0(dk_dk_bool.ite_c) , app0(dk_dk_bool.bool_c) )
dk_dk_bool.iteb_t = { tapp_f = dk_dk_bool.ite_t ; tapp_a = dk_dk_bool.bool_t ; tapp_ca = { clazy = function() return app0(dk_dk_bool.bool_c) end } }
dk_dk_bool.not_c = { clam_f = function (b_c) return app( app( app( app0(dk_dk_bool.iteb_c) , b_c ) , app0(dk_dk_bool.false_c) ) , app0(dk_dk_bool.true_c) ) end }
dk_dk_bool.not_t = { tlam_tty = dk_dk_bool.B_t ; tlam_cty = { clazy = function() return app0(dk_dk_bool.B_c) end } ; tlam_f =  function (b_t, b_c) return { tapp_f = { tapp_f = { tapp_f = dk_dk_bool.iteb_t ; tapp_a = b_t ; tapp_ca = b_c } ; tapp_a = dk_dk_bool.false_t ; tapp_ca = { clazy = function() return app0(dk_dk_bool.false_c) end } } ; tapp_a = dk_dk_bool.true_t ; tapp_ca = { clazy = function() return app0(dk_dk_bool.true_c) end } } end }
dk_dk_bool.and_c = { clam_f = function (x_c) return { clam_f = function (y_c) return app( app( app( app0(dk_dk_bool.iteb_c) , x_c ) , y_c ) , app0(dk_dk_bool.false_c) ) end } end }
dk_dk_bool.and_t = { tlam_tty = dk_dk_bool.B_t ; tlam_cty = { clazy = function() return app0(dk_dk_bool.B_c) end } ; tlam_f =  function (x_t, x_c) return { tlam_tty = dk_dk_bool.B_t ; tlam_cty = { clazy = function() return app0(dk_dk_bool.B_c) end } ; tlam_f =  function (y_t, y_c) return { tapp_f = { tapp_f = { tapp_f = dk_dk_bool.iteb_t ; tapp_a = x_t ; tapp_ca = x_c } ; tapp_a = y_t ; tapp_ca = y_c } ; tapp_a = dk_dk_bool.false_t ; tapp_ca = { clazy = function() return app0(dk_dk_bool.false_c) end } } end } end }
dk_dk_bool.or_c = { clam_f = function (x_c) return { clam_f = function (y_c) return app( app( app( app0(dk_dk_bool.iteb_c) , x_c ) , app0(dk_dk_bool.true_c) ) , y_c ) end } end }
dk_dk_bool.or_t = { tlam_tty = dk_dk_bool.B_t ; tlam_cty = { clazy = function() return app0(dk_dk_bool.B_c) end } ; tlam_f =  function (x_t, x_c) return { tlam_tty = dk_dk_bool.B_t ; tlam_cty = { clazy = function() return app0(dk_dk_bool.B_c) end } ; tlam_f =  function (y_t, y_c) return { tapp_f = { tapp_f = { tapp_f = dk_dk_bool.iteb_t ; tapp_a = x_t ; tapp_ca = x_c } ; tapp_a = dk_dk_bool.true_t ; tapp_ca = { clazy = function() return app0(dk_dk_bool.true_c) end } } ; tapp_a = y_t ; tapp_ca = y_c } end } end }
dk_dk_bool.xor_c = { clam_f = function (x_c) return { clam_f = function (y_c) return app( app( app( app0(dk_dk_bool.iteb_c) , x_c ) , app( app0(dk_dk_bool.not_c) , y_c ) ) , y_c ) end } end }
dk_dk_bool.xor_t = { tlam_tty = dk_dk_bool.B_t ; tlam_cty = { clazy = function() return app0(dk_dk_bool.B_c) end } ; tlam_f =  function (x_t, x_c) return { tlam_tty = dk_dk_bool.B_t ; tlam_cty = { clazy = function() return app0(dk_dk_bool.B_c) end } ; tlam_f =  function (y_t, y_c) return { tapp_f = { tapp_f = { tapp_f = dk_dk_bool.iteb_t ; tapp_a = x_t ; tapp_ca = x_c } ; tapp_a = { tapp_f = dk_dk_bool.not_t ; tapp_a = y_t ; tapp_ca = y_c } ; tapp_ca = { clazy = function() return app( app0(dk_dk_bool.not_c) , y_c ) end } } ; tapp_a = y_t ; tapp_ca = y_c } end } end }
dk_dk_bool.imp_c = { clam_f = function (x_c) return { clam_f = function (y_c) return app( app( app( app0(dk_dk_bool.iteb_c) , x_c ) , y_c ) , app0(dk_dk_bool.true_c) ) end } end }
dk_dk_bool.imp_t = { tlam_tty = dk_dk_bool.B_t ; tlam_cty = { clazy = function() return app0(dk_dk_bool.B_c) end } ; tlam_f =  function (x_t, x_c) return { tlam_tty = dk_dk_bool.B_t ; tlam_cty = { clazy = function() return app0(dk_dk_bool.B_c) end } ; tlam_f =  function (y_t, y_c) return { tapp_f = { tapp_f = { tapp_f = dk_dk_bool.iteb_t ; tapp_a = x_t ; tapp_ca = x_c } ; tapp_a = y_t ; tapp_ca = y_c } ; tapp_a = dk_dk_bool.true_t ; tapp_ca = { clazy = function() return app0(dk_dk_bool.true_c) end } } end } end }
dk_dk_bool.eqv_c = { clam_f = function (x_c) return { clam_f = function (y_c) return app( app( app( app0(dk_dk_bool.iteb_c) , x_c ) , y_c ) , app( app0(dk_dk_bool.not_c) , y_c ) ) end } end }
dk_dk_bool.eqv_t = { tlam_tty = dk_dk_bool.B_t ; tlam_cty = { clazy = function() return app0(dk_dk_bool.B_c) end } ; tlam_f =  function (x_t, x_c) return { tlam_tty = dk_dk_bool.B_t ; tlam_cty = { clazy = function() return app0(dk_dk_bool.B_c) end } ; tlam_f =  function (y_t, y_c) return { tapp_f = { tapp_f = { tapp_f = dk_dk_bool.iteb_t ; tapp_a = x_t ; tapp_ca = x_c } ; tapp_a = y_t ; tapp_ca = y_c } ; tapp_a = { tapp_f = dk_dk_bool.not_t ; tapp_a = y_t ; tapp_ca = y_c } ; tapp_ca = { clazy = function() return app( app0(dk_dk_bool.not_c) , y_c ) end } } end } end }
