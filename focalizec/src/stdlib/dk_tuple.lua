require('dedukti')
dk_dk_tuple = { }
require("cc")
check_ext(dk_cc,'[ Lua ]  cc is undefined.')
dk_dk_tuple.Tuple_c = { cid = "dk_tuple.Tuple" ; args = { } }
dk_dk_tuple.Tuple_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_cc.uT_c) ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } end } }
check_ext(dk_dk_tuple,'[ Lua ]  dk_tuple is undefined.')
dk_dk_tuple.cpl_c = { cid = "dk_tuple.cpl" ; args = { } }
dk_dk_tuple.cpl_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = app0(dk_cc.uT_c) ; cpi_f = function (B_c) return { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (dummy) return { cpi_cty = app( app0(dk_cc.eT_c) , B_c ) ; cpi_f = function (dummy) return app( app0(dk_cc.eT_c) , app( app( app0(dk_dk_tuple.Tuple_c) , A_c ) , B_c ) ) end } end } end } end } }
dk_dk_tuple.fst_c = { cid = "dk_tuple.fst" ; args = { } }
dk_dk_tuple.fst_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = app0(dk_cc.uT_c) ; cpi_f = function (B_c) return { cpi_cty = app( app0(dk_cc.eT_c) , app( app( app0(dk_dk_tuple.Tuple_c) , A_c ) , B_c ) ) ; cpi_f = function (dummy) return app( app0(dk_cc.eT_c) , A_c ) end } end } end } }
dk_dk_tuple.snd_c = { cid = "dk_tuple.snd" ; args = { } }
dk_dk_tuple.snd_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = app0(dk_cc.uT_c) ; cpi_f = function (B_c) return { cpi_cty = app( app0(dk_cc.eT_c) , app( app( app0(dk_dk_tuple.Tuple_c) , A_c ) , B_c ) ) ; cpi_f = function (dummy) return app( app0(dk_cc.eT_c) , B_c ) end } end } end } }
dk_dk_tuple.fst_c = { cid="dk_tuple.fst" ; arity = 3 ; args = { } ; f = function(y1, y2, y3)
local y1 = force2(y1)
local y2 = force2(y2)
local y3 = force2(y3)
if y3.cid == "dk_tuple.cpl" then
local A_c, B_c, a_c, b_c, dummy, dummy = y3.args[1], y3.args[2], y3.args[3], y3.args[4], y1, y2
return a_c
else
return nil
end
end }
dk_dk_tuple.snd_c = { cid="dk_tuple.snd" ; arity = 3 ; args = { } ; f = function(y1, y2, y3)
local y1 = force2(y1)
local y2 = force2(y2)
local y3 = force2(y3)
if y3.cid == "dk_tuple.cpl" then
local A_c, B_c, a_c, b_c, dummy, dummy = y3.args[1], y3.args[2], y3.args[3], y3.args[4], y1, y2
return b_c
else
return nil
end
end }
dk_dk_tuple.dTuple_c = { cid = "dk_tuple.dTuple" ; args = { } }
dk_dk_tuple.dTuple_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } end } }
dk_dk_tuple.dcpl_c = { cid = "dk_tuple.dcpl" ; args = { } }
dk_dk_tuple.dcpl_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } ; cpi_f = function (B_c) return { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (a_c) return { cpi_cty = app( app0(dk_cc.eT_c) , app( B_c , a_c ) ) ; cpi_f = function (dummy) return app( app0(dk_cc.eT_c) , app( app( app0(dk_dk_tuple.dTuple_c) , A_c ) , B_c ) ) end } end } end } end } }
dk_dk_tuple.dfst_c = { cid = "dk_tuple.dfst" ; args = { } }
dk_dk_tuple.dfst_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } ; cpi_f = function (B_c) return { cpi_cty = app( app0(dk_cc.eT_c) , app( app( app0(dk_dk_tuple.dTuple_c) , A_c ) , B_c ) ) ; cpi_f = function (dummy) return app( app0(dk_cc.eT_c) , A_c ) end } end } end } }
dk_dk_tuple.dsnd_c = { cid = "dk_tuple.dsnd" ; args = { } }
dk_dk_tuple.dsnd_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_cc.uT_c) end } ; cpi_f = function (A_c) return { cpi_cty = { cpi_cty = app( app0(dk_cc.eT_c) , A_c ) ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } ; cpi_f = function (B_c) return { cpi_cty = app( app0(dk_cc.eT_c) , app( app( app0(dk_dk_tuple.dTuple_c) , A_c ) , B_c ) ) ; cpi_f = function (t_c) return app( app0(dk_cc.eT_c) , app( B_c , app( app( app( app0(dk_dk_tuple.dfst_c) , A_c ) , B_c ) , t_c ) ) ) end } end } end } }
dk_dk_tuple.dfst_c = { cid="dk_tuple.dfst" ; arity = 3 ; args = { } ; f = function(y1, y2, y3)
local y1 = force2(y1)
local y2 = force2(y2)
local y3 = force2(y3)
if y3.cid == "dk_tuple.dcpl" then
local A_c, B_c, a_c, b_c, dummy, dummy = y3.args[1], y3.args[2], y3.args[3], y3.args[4], y1, y2
return a_c
else
return nil
end
end }
dk_dk_tuple.dsnd_c = { cid="dk_tuple.dsnd" ; arity = 3 ; args = { } ; f = function(y1, y2, y3)
local y1 = force2(y1)
local y2 = force2(y2)
local y3 = force2(y3)
if y3.cid == "dk_tuple.dcpl" then
local A_c, B_c, a_c, b_c, dummy, dummy = y3.args[1], y3.args[2], y3.args[3], y3.args[4], y1, y2
return b_c
else
return nil
end
end }
