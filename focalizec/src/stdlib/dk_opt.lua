require('dedukti')
dk_dk_opt = { }
require("cc")
check_ext(dk_cc,'[ Lua ]  cc is undefined.')
dk_dk_opt.option_c = { cid = "dk_opt.option" ; args = { } }
dk_dk_opt.option_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } }
check_ext(dk_dk_opt,'[ Lua ]  dk_opt is undefined.')
dk_dk_opt.None_c = { cid = "dk_opt.None" ; args = { } }
dk_dk_opt.None_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return app( app0(dk_cc.eT_c) , app( app0(dk_dk_opt.option_c) , A_c ) ) end } }
dk_dk_opt.Some_c = { cid = "dk_opt.Some" ; args = { } }
dk_dk_opt.Some_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (dummy) return app( app0(dk_cc.eT_c) , app( app0(dk_dk_opt.option_c) , A_c ) ) end } end } }
dk_dk_opt.match_option_c = { cid = "dk_opt.match_option" ; args = { } }
dk_dk_opt.match_option_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = { cpi_cty = app( app0(dk_cc.eT_c) , app( app0(dk_dk_opt.option_c) , A_c ) ) ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } ; cpi_f = function (P_c) return { cpi_cty = app( app0(dk_cc.eT_c) , app( P_c , app( app0(dk_dk_opt.None_c) , A_c ) ) ) ; cpi_f = function (dummy) return { cpi_cty = { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (a_c) return app( app0(dk_cc.eT_c) , app( P_c , app( app( app0(dk_dk_opt.Some_c) , A_c ) , a_c ) ) ) end } ; cpi_f = function (dummy) return { cpi_cty = app( app0(dk_cc.eT_c) , app( app0(dk_dk_opt.option_c) , A_c ) ) ; cpi_f = function (o_c) return app( app0(dk_cc.eT_c) , app( P_c , o_c ) ) end } end } end } end } end } }
dk_dk_opt.simple_match_option_c = { clam_f = function (A_c) return { clam_f = function (return_c) return app( app( app0(dk_dk_opt.match_option_c) , A_c ) , { clam_f = function (_x_c) return return_c end } ) end } end }
dk_dk_opt.simple_match_option_t = { tlam_tty = dk_cc.uT_t ; tlam_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; tlam_f =  function (A_t, A_c) return { tlam_tty = dk_cc.uT_t ; tlam_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; tlam_f =  function (return_t, return_c) return { tapp_f = { tapp_f = dk_dk_opt.match_option_t ; tapp_a = A_t ; tapp_ca = A_c } ; tapp_a = { tlam_tty = { tapp_f = dk_cc.eT_t ; tapp_a = { tapp_f = dk_dk_opt.option_t ; tapp_a = A_t ; tapp_ca = A_c } ; tapp_ca = { clazy = function() return app( app0(dk_dk_opt.option_c) , A_c ) end } } ; tlam_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app0(dk_dk_opt.option_c) , A_c ) ) end } ; tlam_f =  function (_x_t, _x_c) return return_t end } ; tapp_ca = { clam_f = function (_x_c) return return_c end } } end } end }
dk_dk_opt.match_option_c = { cid="dk_opt.match_option" ; arity = 5 ; args = { } ; f = function(y1, y2, y3, y4, y5)
local y1 = force2(y1)
local y2 = force2(y2)
local y3 = force2(y3)
local y4 = force2(y4)
local y5 = force2(y5)
if y5.cid == "dk_opt.None" then
local A_c, dummy, P_c, Hnone_c, Hsome_c = y5.args[1], y1, y2, y3, y4
return Hnone_c
elseif y5.cid == "dk_opt.Some" then
local A_c, a_c, dummy, P_c, Hnone_c, Hsome_c = y5.args[1], y5.args[2], y1, y2, y3, y4
return app( Hsome_c , a_c )
else
return nil
end
end }
