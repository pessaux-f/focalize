require('dedukti')
dk_dk_list = { }
require("cc")
require("dk_logic")
check_ext(dk_cc,'[ Lua ]  cc is undefined.')
dk_dk_list.list_c = { cid = "dk_list.list" ; args = { } }
dk_dk_list.list_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } }
check_ext(dk_dk_list,'[ Lua ]  dk_list is undefined.')
dk_dk_list.nil_c = { cid = "dk_list.nil" ; args = { } }
dk_dk_list.nil_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return app( app0(dk_cc.eT_c) , app( app0(dk_dk_list.list_c) , A_c ) ) end } }
dk_dk_list.cons_c = { cid = "dk_list.cons" ; args = { } }
dk_dk_list.cons_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (dummy) return { cpi_cty = app( app0(dk_cc.eT_c) , app( app0(dk_dk_list.list_c) , A_c ) ) ; cpi_f = function (dummy) return app( app0(dk_cc.eT_c) , app( app0(dk_dk_list.list_c) , A_c ) ) end } end } end } }
dk_dk_list.match_c = { cid = "dk_list.match" ; args = { } }
dk_dk_list.match_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = { cpi_cty = app( app0(dk_cc.eT_c) , app( app0(dk_dk_list.list_c) , A_c ) ) ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } ; cpi_f = function (P_c) return { cpi_cty = app( app0(dk_cc.eT_c) , app( P_c , app( app0(dk_dk_list.nil_c) , A_c ) ) ) ; cpi_f = function (dummy) return { cpi_cty = { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (a_c) return { cpi_cty = app( app0(dk_cc.eT_c) , app( app0(dk_dk_list.list_c) , A_c ) ) ; cpi_f = function (l_c) return app( app0(dk_cc.eT_c) , app( P_c , app( app( app( app0(dk_dk_list.cons_c) , A_c ) , a_c ) , l_c ) ) ) end } end } ; cpi_f = function (dummy) return { cpi_cty = app( app0(dk_cc.eT_c) , app( app0(dk_dk_list.list_c) , A_c ) ) ; cpi_f = function (l_c) return app( app0(dk_cc.eT_c) , app( P_c , l_c ) ) end } end } end } end } end } }
dk_dk_list.match_c = { cid="dk_list.match" ; arity = 5 ; args = { } ; f = function(y1, y2, y3, y4, y5)
local y1 = force2(y1)
local y2 = force2(y2)
local y3 = force2(y3)
local y4 = force2(y4)
local y5 = force2(y5)
if y5.cid == "dk_list.nil" then
local A_c, dummy, P_c, Hnil_c, Hcons_c = y5.args[1], y1, y2, y3, y4
return Hnil_c
elseif y5.cid == "dk_list.cons" then
local A_c, a_c, l_c, dummy, P_c, Hnil_c, Hcons_c = y5.args[1], y5.args[2], y5.args[3], y1, y2, y3, y4
return app( app( Hcons_c , a_c ) , l_c )
else
return nil
end
end }
dk_dk_list.simple_match_c = { clam_f = function (A_c) return { clam_f = function (return_c) return app( app( app0(dk_dk_list.match_c) , A_c ) , { clam_f = function (_x_c) return return_c end } ) end } end }
dk_dk_list.simple_match_t = { tlam_tty = dk_cc.uT_t ; tlam_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; tlam_f =  function (A_t, A_c) return { tlam_tty = dk_cc.uT_t ; tlam_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; tlam_f =  function (return_t, return_c) return { tapp_f = { tapp_f = dk_dk_list.match_t ; tapp_a = A_t ; tapp_ca = A_c } ; tapp_a = { tlam_tty = { tapp_f = dk_cc.eT_t ; tapp_a = { tapp_f = dk_dk_list.list_t ; tapp_a = A_t ; tapp_ca = A_c } ; tapp_ca = { clazy = function() return app( app0(dk_dk_list.list_c) , A_c ) end } } ; tlam_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app0(dk_dk_list.list_c) , A_c ) ) end } ; tlam_f =  function (_x_t, _x_c) return return_t end } ; tapp_ca = { clam_f = function (_x_c) return return_c end } } end } end }
dk_dk_list.map_c = { cid = "dk_list.map" ; args = { } }
dk_dk_list.map_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = app0(dk_cc.uT_c) ; cpi_f = function (B_c) return { cpi_cty = { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (dummy) return app( app0(dk_cc.eT_c) , B_c ) end } ; cpi_f = function (dummy) return { cpi_cty = app( app0(dk_cc.eT_c) , app( app0(dk_dk_list.list_c) , A_c ) ) ; cpi_f = function (dummy) return app( app0(dk_cc.eT_c) , app( app0(dk_dk_list.list_c) , B_c ) ) end } end } end } end } }
dk_dk_list.map_c = { cid="dk_list.map" ; arity = 4 ; args = { } ; f = function(y1, y2, y3, y4)
local y1 = force2(y1)
local y2 = force2(y2)
local y3 = force2(y3)
local y4 = force2(y4)
if y4.cid == "dk_list.nil" then
local A_c, A_c, B_c, f_c = y4.args[1], y1, y2, y3
return app( app0(dk_dk_list.nil_c) , B_c )
elseif y4.cid == "dk_list.cons" then
local A_c, a_c, l_c, A_c, B_c, f_c = y4.args[1], y4.args[2], y4.args[3], y1, y2, y3
return app( app( app( app0(dk_dk_list.cons_c) , B_c ) , app( f_c , a_c ) ) , app( app( app( app( app0(dk_dk_list.map_c) , A_c ) , B_c ) , f_c ) , l_c ) )
elseif y4.cid == "dk_list.map" then
local A_c, B_c, f_c, l_c, B_c, C_c, g_c = y4.args[1], y4.args[2], y4.args[3], y4.args[4], y1, y2, y3
return app( app( app( app( app0(dk_dk_list.map_c) , A_c ) , C_c ) , { clam_f = function (x_c) return app( g_c , app( f_c , x_c ) ) end } ) , l_c )
else
return nil
end
end }
check_ext(dk_dk_logic,'[ Lua ]  dk_logic is undefined.')
dk_dk_list.map_id_c = { cid = "dk_list.map_id" ; args = { } }
dk_dk_list.map_id_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = app( app0(dk_cc.eT_c) , app( app0(dk_dk_list.list_c) , A_c ) ) ; cpi_f = function (l_c) return app( app0(dk_dk_logic.eP_c) , app( app( app( app0(dk_dk_logic.equal_c) , app( app0(dk_dk_list.list_c) , A_c ) ) , app( app( app( app( app0(dk_dk_list.map_c) , A_c ) , A_c ) , { clam_f = function (x_c) return x_c end } ) , l_c ) ) , l_c ) ) end } end } }
dk_dk_list.map_id_c = { cid="dk_list.map_id" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y2.cid == "dk_list.nil" then
local A_c, dummy = y2.args[1], y1
return app( app( app0(dk_dk_logic.refl_c) , app( app0(dk_dk_list.list_c) , A_c ) ) , app( app0(dk_dk_list.nil_c) , A_c ) )
elseif y2.cid == "dk_list.cons" then
local A_c, a_c, l_c, dummy = y2.args[1], y2.args[2], y2.args[3], y1
return app( app( app( app( app( app( app0(dk_dk_logic.equal_congr_c) , app( app0(dk_dk_list.list_c) , A_c ) ) , app( app0(dk_dk_list.list_c) , A_c ) ) , app( app( app0(dk_dk_list.cons_c) , A_c ) , a_c ) ) , app( app( app( app( app0(dk_dk_list.map_c) , A_c ) , A_c ) , { clam_f = function (x_c) return x_c end } ) , l_c ) ) , l_c ) , app( app( app0(dk_dk_list.map_id_c) , A_c ) , l_c ) )
else
return nil
end
end }
dk_dk_list.dlist_c = { cid = "dk_list.dlist" ; args = { } }
dk_dk_list.dlist_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } end } }
dk_dk_list.dnil_c = { cid = "dk_list.dnil" ; args = { } }
dk_dk_list.dnil_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (a_c) return app( app0(dk_cc.eT_c) , app( app( app0(dk_dk_list.dlist_c) , A_c ) , a_c ) ) end } end } }
dk_dk_list.dcons_c = { cid = "dk_list.dcons" ; args = { } }
dk_dk_list.dcons_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (a_c) return { cpi_cty = { cpi_cty = app( app0(dk_cc.eT_c) , app( app( app0(dk_dk_list.dlist_c) , A_c ) , a_c ) ) ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } ; cpi_f = function (f_c) return { cpi_cty = app( app0(dk_cc.eT_c) , app( app( app0(dk_dk_list.dlist_c) , A_c ) , a_c ) ) ; cpi_f = function (l_c) return { cpi_cty = app( app0(dk_cc.eT_c) , app( f_c , l_c ) ) ; cpi_f = function (dummy) return app( app0(dk_cc.eT_c) , app( app( app0(dk_dk_list.dlist_c) , A_c ) , a_c ) ) end } end } end } end } end } }
