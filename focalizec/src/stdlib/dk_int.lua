require('dedukti')
dk_dk_int = { }
require("cc")
require("dk_nat")
check_ext(dk_cc,'[ Lua ]  cc is undefined.')
check_ext(dk_dk_nat,'[ Lua ]  dk_nat is undefined.')
dk_dk_int.N_c = app( app0(dk_cc.eT_c) , app0(dk_dk_nat.Nat_c) )
dk_dk_int.N_t = { tapp_f = dk_cc.eT_t ; tapp_a = dk_dk_nat.Nat_t ; tapp_ca = { clazy = function() return app0(dk_dk_nat.Nat_c) end } }
dk_dk_int.O_c = app0(dk_dk_nat.O_c)
dk_dk_int.O_t = dk_dk_nat.O_t
dk_dk_int.S_c = app0(dk_dk_nat.S_c)
dk_dk_int.S_t = dk_dk_nat.S_t
require("dk_list")
require("dk_bool")
check_ext(dk_dk_bool,'[ Lua ]  dk_bool is undefined.')
dk_dk_int.B_c = app( app0(dk_cc.eT_c) , app0(dk_dk_bool.bool_c) )
dk_dk_int.B_t = { tapp_f = dk_cc.eT_t ; tapp_a = dk_dk_bool.bool_t ; tapp_ca = { clazy = function() return app0(dk_dk_bool.bool_c) end } }
dk_dk_int.int_c = { cid = "dk_int.int" ; args = { } }
dk_dk_int.int_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
check_ext(dk_dk_int,'[ Lua ]  dk_int is undefined.')
dk_dk_int.I_c = app( app0(dk_cc.eT_c) , app0(dk_dk_int.int_c) )
dk_dk_int.I_t = { tapp_f = dk_cc.eT_t ; tapp_a = dk_dk_int.int_t ; tapp_ca = { clazy = function() return app0(dk_dk_int.int_c) end } }
dk_dk_int.make_c = { cid = "dk_int.make" ; args = { } }
dk_dk_int.make_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_int.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_int.N_c) ; cpi_f = function (dummy) return app0(dk_dk_int.I_c) end } end } }
dk_dk_int.make_c = { cid="dk_int.make" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "dk_int.S" then
if y2.cid == "dk_int.S" then
local m_c, n_c = y2.args[1], y1.args[1]
return app( app( app0(dk_dk_int.make_c) , n_c ) , m_c )
else
return nil
end
else
return nil
end
end }
dk_dk_int.nat_abs_c = { cid = "dk_int.nat_abs" ; args = { } }
dk_dk_int.nat_abs_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_int.I_c) end } ; cpi_f = function (dummy) return app0(dk_dk_int.N_c) end } }
dk_dk_int.nat_abs_c = { cid="dk_int.nat_abs" ; arity = 1 ; args = { } ; f = function(y1)
local y1 = force2(y1)
if y1.cid == "dk_int.make" then
if y1.args[1].cid == "dk_int.O" then
local m_c = y1.args[2]
return m_c
else
if y1.args[2].cid == "dk_int.O" then
local n_c = y1.args[1]
return n_c
else
return nil
end
end
else
return nil
end
end }
dk_dk_int.leq_c = { cid = "dk_int.leq" ; args = { } }
dk_dk_int.leq_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_int.I_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_int.I_c) ; cpi_f = function (dummy) return app0(dk_dk_int.B_c) end } end } }
dk_dk_int.leq_c = { cid="dk_int.leq" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "dk_int.make" then
if y2.cid == "dk_int.make" then
local p_c, q_c, n_c, m_c = y2.args[1], y2.args[2], y1.args[1], y1.args[2]
return app( app( app0(dk_dk_nat.leq_c) , app( app( app0(dk_dk_nat.plus_c) , n_c ) , q_c ) ) , app( app( app0(dk_dk_nat.plus_c) , m_c ) , p_c ) )
else
return nil
end
else
return nil
end
end }
dk_dk_int.lt_c = { cid = "dk_int.lt" ; args = { } }
dk_dk_int.lt_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_int.I_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_int.I_c) ; cpi_f = function (dummy) return app0(dk_dk_int.B_c) end } end } }
dk_dk_int.lt_c = { cid="dk_int.lt" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "dk_int.make" then
if y2.cid == "dk_int.make" then
local p_c, q_c, n_c, m_c = y2.args[1], y2.args[2], y1.args[1], y1.args[2]
return app( app( app0(dk_dk_nat.lt_c) , app( app( app0(dk_dk_nat.plus_c) , n_c ) , q_c ) ) , app( app( app0(dk_dk_nat.plus_c) , m_c ) , p_c ) )
else
return nil
end
else
return nil
end
end }
dk_dk_int.geq_c = { cid = "dk_int.geq" ; args = { } }
dk_dk_int.geq_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_int.I_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_int.I_c) ; cpi_f = function (dummy) return app0(dk_dk_int.B_c) end } end } }
dk_dk_int.geq_c = { cid="dk_int.geq" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local i_c, j_c = y1, y2
return app( app( app0(dk_dk_int.leq_c) , j_c ) , i_c )
end }
dk_dk_int.gt_c = { cid = "dk_int.gt" ; args = { } }
dk_dk_int.gt_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_int.I_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_int.I_c) ; cpi_f = function (dummy) return app0(dk_dk_int.B_c) end } end } }
dk_dk_int.gt_c = { cid="dk_int.gt" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local i_c, j_c = y1, y2
return app( app( app0(dk_dk_int.lt_c) , j_c ) , i_c )
end }
dk_dk_int.eq_c = { cid = "dk_int.eq" ; args = { } }
dk_dk_int.eq_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_int.I_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_int.I_c) ; cpi_f = function (dummy) return app0(dk_dk_int.B_c) end } end } }
dk_dk_int.eq_c = { cid="dk_int.eq" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local i_c, j_c = y1, y2
return app( app( app0(dk_dk_bool.and_c) , app( app( app0(dk_dk_int.leq_c) , i_c ) , j_c ) ) , app( app( app0(dk_dk_int.geq_c) , i_c ) , j_c ) )
end }
dk_dk_int.plus_c = { cid = "dk_int.plus" ; args = { } }
dk_dk_int.plus_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_int.I_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_int.I_c) ; cpi_f = function (dummy) return app0(dk_dk_int.I_c) end } end } }
dk_dk_int.plus_c = { cid="dk_int.plus" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "dk_int.make" then
if y2.cid == "dk_int.make" then
local p_c, q_c, n_c, m_c = y2.args[1], y2.args[2], y1.args[1], y1.args[2]
return app( app( app0(dk_dk_int.make_c) , app( app( app0(dk_dk_nat.plus_c) , n_c ) , p_c ) ) , app( app( app0(dk_dk_nat.plus_c) , m_c ) , q_c ) )
else
return nil
end
else
return nil
end
end }
dk_dk_int.opp_c = { cid = "dk_int.opp" ; args = { } }
dk_dk_int.opp_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_int.I_c) end } ; cpi_f = function (dummy) return app0(dk_dk_int.I_c) end } }
dk_dk_int.opp_c = { cid="dk_int.opp" ; arity = 1 ; args = { } ; f = function(y1)
local y1 = force2(y1)
if y1.cid == "dk_int.make" then
local n_c, m_c = y1.args[1], y1.args[2]
return app( app( app0(dk_dk_int.make_c) , m_c ) , n_c )
else
return nil
end
end }
dk_dk_int.sub_c = { cid = "dk_int.sub" ; args = { } }
dk_dk_int.sub_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_int.I_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_int.I_c) ; cpi_f = function (dummy) return app0(dk_dk_int.I_c) end } end } }
dk_dk_int.sub_c = { cid="dk_int.sub" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local i_c, j_c = y1, y2
return app( app( app0(dk_dk_int.plus_c) , i_c ) , app( app0(dk_dk_int.opp_c) , j_c ) )
end }
dk_dk_int.mult_c = { cid = "dk_int.mult" ; args = { } }
dk_dk_int.mult_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_int.I_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_int.I_c) ; cpi_f = function (dummy) return app0(dk_dk_int.I_c) end } end } }
dk_dk_int.mult_c = { cid="dk_int.mult" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "dk_int.make" then
if y2.cid == "dk_int.make" then
local p_c, q_c, n_c, m_c = y2.args[1], y2.args[2], y1.args[1], y1.args[2]
return app( app( app0(dk_dk_int.make_c) , app( app( app0(dk_dk_nat.plus_c) , app( app( app0(dk_dk_nat.mult_c) , n_c ) , p_c ) ) , app( app( app0(dk_dk_nat.mult_c) , m_c ) , q_c ) ) ) , app( app( app0(dk_dk_nat.plus_c) , app( app( app0(dk_dk_nat.mult_c) , n_c ) , q_c ) ) , app( app( app0(dk_dk_nat.mult_c) , m_c ) , p_c ) ) )
else
return nil
end
else
return nil
end
end }
dk_dk_int.max_c = { cid = "dk_int.max" ; args = { } }
dk_dk_int.max_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_int.I_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_int.I_c) ; cpi_f = function (dummy) return app0(dk_dk_int.I_c) end } end } }
dk_dk_int.max_c = { cid="dk_int.max" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local m_c, n_c = y1, y2
return app( app( app( app( app0(dk_dk_bool.ite_c) , app0(dk_dk_int.int_c) ) , app( app( app0(dk_dk_int.leq_c) , m_c ) , n_c ) ) , n_c ) , m_c )
end }
dk_dk_int.min_c = { cid = "dk_int.min" ; args = { } }
dk_dk_int.min_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_int.I_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_int.I_c) ; cpi_f = function (dummy) return app0(dk_dk_int.I_c) end } end } }
dk_dk_int.min_c = { cid="dk_int.min" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local m_c, n_c = y1, y2
return app( app( app( app( app0(dk_dk_bool.ite_c) , app0(dk_dk_int.int_c) ) , app( app( app0(dk_dk_int.leq_c) , m_c ) , n_c ) ) , m_c ) , n_c )
end }
dk_dk_int.abs_c = { cid = "dk_int.abs" ; args = { } }
dk_dk_int.abs_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_int.I_c) end } ; cpi_f = function (dummy) return app0(dk_dk_int.I_c) end } }
dk_dk_int.abs_c = { cid="dk_int.abs" ; arity = 1 ; args = { } ; f = function(y1)
local y1 = force2(y1)
local i_c = y1
return app( app( app0(dk_dk_int.make_c) , app( app0(dk_dk_int.nat_abs_c) , i_c ) ) , app0(dk_dk_int.O_c) )
end }
dk_dk_int.mod_c = { cid = "dk_int.mod" ; args = { } }
dk_dk_int.mod_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_int.I_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_int.I_c) ; cpi_f = function (dummy) return app0(dk_dk_int.I_c) end } end } }
dk_dk_int.mod_c = { cid="dk_int.mod" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "dk_int.make" then
if y2.cid == "dk_int.make" then
if y2.args[2].cid == "dk_int.O" then
local p_c, m_c, n_c = y2.args[1], y1.args[1], y1.args[2]
return app( app( app0(dk_dk_int.make_c) , app( app( app0(dk_dk_nat.mod_c) , m_c ) , p_c ) ) , app( app( app0(dk_dk_nat.mod_c) , n_c ) , p_c ) )
else
if y2.args[1].cid == "dk_int.O" then
local p_c, m_c, n_c = y2.args[2], y1.args[1], y1.args[2]
return app( app( app0(dk_dk_int.make_c) , app( app( app0(dk_dk_nat.mod_c) , m_c ) , p_c ) ) , app( app( app0(dk_dk_nat.mod_c) , n_c ) , p_c ) )
else
return nil
end
end
else
return nil
end
else
return nil
end
end }
dk_dk_int.quo_c = { cid = "dk_int.quo" ; args = { } }
dk_dk_int.quo_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_int.I_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_int.I_c) ; cpi_f = function (dummy) return app0(dk_dk_int.I_c) end } end } }
dk_dk_int.quo_c = { cid="dk_int.quo" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "dk_int.make" then
if y1.args[2].cid == "dk_int.O" then
if y2.cid == "dk_int.make" then
if y2.args[1].cid == "dk_int.O" then
if y1.args[1].cid == "dk_int.O" then
if y2.args[2].cid == "dk_int.O" then
return app( app( app0(dk_dk_int.make_c) , app( app( app0(dk_dk_nat.quo_c) , m_c ) , p_c ) ) , app0(dk_dk_int.O_c) )
else
local p_c = y2.args[2]
return app( app( app0(dk_dk_int.make_c) , app( app( app0(dk_dk_nat.quo_c) , m_c ) , p_c ) ) , app0(dk_dk_int.O_c) )
end
else
local p_c, m_c = y2.args[2], y1.args[1]
return app( app( app0(dk_dk_int.make_c) , app0(dk_dk_int.O_c) ) , app( app( app0(dk_dk_nat.quo_c) , m_c ) , p_c ) )
end
else
if y2.args[2].cid == "dk_int.O" then
if y1.args[1].cid == "dk_int.O" then
local p_c = y2.args[1]
return app( app( app0(dk_dk_int.make_c) , app0(dk_dk_int.O_c) ) , app( app( app0(dk_dk_nat.quo_c) , m_c ) , p_c ) )
else
local p_c, m_c = y2.args[1], y1.args[1]
return app( app( app0(dk_dk_int.make_c) , app( app( app0(dk_dk_nat.quo_c) , m_c ) , p_c ) ) , app0(dk_dk_int.O_c) )
end
else
return nil
end
end
else
return nil
end
else
if y1.args[1].cid == "dk_int.O" then
if y2.cid == "dk_int.make" then
if y2.args[1].cid == "dk_int.O" then
local p_c, m_c = y2.args[2], y1.args[2]
return app( app( app0(dk_dk_int.make_c) , app( app( app0(dk_dk_nat.quo_c) , m_c ) , p_c ) ) , app0(dk_dk_int.O_c) )
else
if y2.args[2].cid == "dk_int.O" then
local p_c, m_c = y2.args[1], y1.args[2]
return app( app( app0(dk_dk_int.make_c) , app0(dk_dk_int.O_c) ) , app( app( app0(dk_dk_nat.quo_c) , m_c ) , p_c ) )
else
return nil
end
end
else
return nil
end
else
return nil
end
end
else
return nil
end
end }
