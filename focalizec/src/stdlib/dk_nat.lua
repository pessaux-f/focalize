require('dedukti')
dk_dk_nat = { }
require("cc")
require("dk_list")
require("dk_bool")
check_ext(dk_cc,'[ Lua ]  cc is undefined.')
check_ext(dk_dk_bool,'[ Lua ]  dk_bool is undefined.')
dk_dk_nat.B_c = app( app0(dk_cc.eT_c) , app0(dk_dk_bool.bool_c) )
dk_dk_nat.B_t = { tapp_f = dk_cc.eT_t ; tapp_a = dk_dk_bool.bool_t ; tapp_ca = { clazy = function() return app0(dk_dk_bool.bool_c) end } }
dk_dk_nat.Nat_c = { cid = "dk_nat.Nat" ; args = { } }
dk_dk_nat.Nat_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
check_ext(dk_dk_nat,'[ Lua ]  dk_nat is undefined.')
dk_dk_nat.N_c = app( app0(dk_cc.eT_c) , app0(dk_dk_nat.Nat_c) )
dk_dk_nat.N_t = { tapp_f = dk_cc.eT_t ; tapp_a = dk_dk_nat.Nat_t ; tapp_ca = { clazy = function() return app0(dk_dk_nat.Nat_c) end } }
dk_dk_nat.O_c = { cid = "dk_nat.O" ; args = { } }
dk_dk_nat.O_t = { tbox_cty = { clazy = function() return app0(dk_dk_nat.N_c) end } }
dk_dk_nat.S_c = { cid = "dk_nat.S" ; args = { } }
dk_dk_nat.S_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_nat.N_c) end } ; cpi_f = function (dummy) return app0(dk_dk_nat.N_c) end } }
dk_dk_nat.lt_c = { cid = "dk_nat.lt" ; args = { } }
dk_dk_nat.lt_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_nat.N_c) ; cpi_f = function (dummy) return app0(dk_dk_nat.B_c) end } end } }
dk_dk_nat.lt_c = { cid="dk_nat.lt" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "dk_nat.O" then
if y2.cid == "dk_nat.S" then
local m_c = y2.args[1]
return app0(dk_dk_bool.true_c)
elseif y2.cid == "dk_nat.O" then
return app0(dk_dk_bool.false_c)
else
return nil
end
elseif y1.cid == "dk_nat.S" then
if y2.cid == "dk_nat.S" then
local m_c, n_c = y2.args[1], y1.args[1]
return app( app( app0(dk_dk_nat.lt_c) , n_c ) , m_c )
elseif y2.cid == "dk_nat.O" then
local dummy_c = y1.args[1]
return app0(dk_dk_bool.false_c)
else
return nil
end
else
if y2.cid == "dk_nat.O" then
local n_c = y1
return app0(dk_dk_bool.false_c)
else
return nil
end
end
end }
dk_dk_nat.gt_c = { cid = "dk_nat.gt" ; args = { } }
dk_dk_nat.gt_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_nat.N_c) ; cpi_f = function (dummy) return app0(dk_dk_nat.B_c) end } end } }
dk_dk_nat.gt_c = { cid="dk_nat.gt" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local n_c, m_c = y1, y2
return app( app( app0(dk_dk_nat.lt_c) , m_c ) , n_c )
end }
dk_dk_nat.leq_c = { cid = "dk_nat.leq" ; args = { } }
dk_dk_nat.leq_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_nat.N_c) ; cpi_f = function (dummy) return app0(dk_dk_nat.B_c) end } end } }
dk_dk_nat.leq_c = { cid="dk_nat.leq" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "dk_nat.O" then
local m_c = y2
return app0(dk_dk_bool.true_c)
elseif y1.cid == "dk_nat.S" then
if y2.cid == "dk_nat.S" then
local m_c, n_c = y2.args[1], y1.args[1]
return app( app( app0(dk_dk_nat.leq_c) , n_c ) , m_c )
elseif y2.cid == "dk_nat.O" then
local n_c = y1.args[1]
return app0(dk_dk_bool.false_c)
else
return nil
end
else
return nil
end
end }
dk_dk_nat.geq_c = { cid = "dk_nat.geq" ; args = { } }
dk_dk_nat.geq_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_nat.N_c) ; cpi_f = function (dummy) return app0(dk_dk_nat.B_c) end } end } }
dk_dk_nat.geq_c = { cid="dk_nat.geq" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local n_c, m_c = y1, y2
return app( app( app0(dk_dk_nat.leq_c) , m_c ) , n_c )
end }
dk_dk_nat.eq_c = { cid = "dk_nat.eq" ; args = { } }
dk_dk_nat.eq_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_nat.N_c) ; cpi_f = function (dummy) return app0(dk_dk_nat.B_c) end } end } }
dk_dk_nat.eq_c = { cid="dk_nat.eq" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local n_c, m_c = y1, y2
return app( app( app0(dk_dk_bool.and_c) , app( app( app0(dk_dk_nat.leq_c) , n_c ) , m_c ) ) , app( app( app0(dk_dk_nat.geq_c) , n_c ) , m_c ) )
end }
dk_dk_nat.plus_c = { cid = "dk_nat.plus" ; args = { } }
dk_dk_nat.plus_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_nat.N_c) ; cpi_f = function (dummy) return app0(dk_dk_nat.N_c) end } end } }
dk_dk_nat.plus_c = { cid="dk_nat.plus" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "dk_nat.O" then
local m_c = y2
return m_c
elseif y1.cid == "dk_nat.S" then
local n_c, m_c = y1.args[1], y2
return app( app0(dk_dk_nat.S_c) , app( app( app0(dk_dk_nat.plus_c) , n_c ) , m_c ) )
else
return nil
end
end }
dk_dk_nat.mult_c = { cid = "dk_nat.mult" ; args = { } }
dk_dk_nat.mult_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_nat.N_c) ; cpi_f = function (dummy) return app0(dk_dk_nat.N_c) end } end } }
dk_dk_nat.mult_c = { cid="dk_nat.mult" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "dk_nat.O" then
local m_c = y2
return app0(dk_dk_nat.O_c)
elseif y1.cid == "dk_nat.S" then
local n_c, m_c = y1.args[1], y2
return app( app( app0(dk_dk_nat.plus_c) , app( app( app0(dk_dk_nat.mult_c) , n_c ) , m_c ) ) , m_c )
else
return nil
end
end }
dk_dk_nat.max_c = { cid = "dk_nat.max" ; args = { } }
dk_dk_nat.max_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_nat.N_c) ; cpi_f = function (dummy) return app0(dk_dk_nat.N_c) end } end } }
dk_dk_nat.max_c = { cid="dk_nat.max" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local m_c, n_c = y1, y2
return app( app( app( app( app0(dk_dk_bool.ite_c) , app0(dk_dk_nat.Nat_c) ) , app( app( app0(dk_dk_nat.leq_c) , m_c ) , n_c ) ) , n_c ) , m_c )
end }
dk_dk_nat.min_c = { cid = "dk_nat.min" ; args = { } }
dk_dk_nat.min_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_nat.N_c) ; cpi_f = function (dummy) return app0(dk_dk_nat.N_c) end } end } }
dk_dk_nat.min_c = { cid="dk_nat.min" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local m_c, n_c = y1, y2
return app( app( app( app( app0(dk_dk_bool.ite_c) , app0(dk_dk_nat.Nat_c) ) , app( app( app0(dk_dk_nat.leq_c) , m_c ) , n_c ) ) , m_c ) , n_c )
end }
dk_dk_nat.mod_aux_c = { cid = "dk_nat.mod_aux" ; args = { } }
dk_dk_nat.mod_aux_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_nat.N_c) ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_nat.N_c) ; cpi_f = function (dummy) return app0(dk_dk_nat.N_c) end } end } end } }
dk_dk_nat.mod_aux_c = { cid="dk_nat.mod_aux" ; arity = 3 ; args = { } ; f = function(y1, y2, y3)
local y1 = force2(y1)
local y2 = force2(y2)
local y3 = force2(y3)
if y1.cid == "dk_nat.O" then
local m_c, r_c = y2, y3
return r_c
elseif y1.cid == "dk_nat.S" then
local n_c, m_c, r_c = y1.args[1], y2, y3
return app( app( app( app0(dk_dk_nat.mod_aux_c) , n_c ) , m_c ) , app( app( app( app( app0(dk_dk_bool.ite_c) , app0(dk_dk_nat.Nat_c) ) , app( app( app0(dk_dk_nat.lt_c) , app( app0(dk_dk_nat.S_c) , r_c ) ) , m_c ) ) , app( app0(dk_dk_nat.S_c) , r_c ) ) , app0(dk_dk_nat.O_c) ) )
else
return nil
end
end }
dk_dk_nat.mod_c = { cid = "dk_nat.mod" ; args = { } }
dk_dk_nat.mod_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_nat.N_c) ; cpi_f = function (dummy) return app0(dk_dk_nat.N_c) end } end } }
dk_dk_nat.mod_c = { cid="dk_nat.mod" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local n_c, m_c = y1, y2
return app( app( app( app0(dk_dk_nat.mod_aux_c) , n_c ) , m_c ) , app0(dk_dk_nat.O_c) )
end }
dk_dk_nat.quo_aux_c = { cid = "dk_nat.quo_aux" ; args = { } }
dk_dk_nat.quo_aux_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_nat.N_c) ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_nat.N_c) ; cpi_f = function (dummy) return app0(dk_dk_nat.N_c) end } end } end } }
dk_dk_nat.quo_aux_c = { cid="dk_nat.quo_aux" ; arity = 3 ; args = { } ; f = function(y1, y2, y3)
local y1 = force2(y1)
local y2 = force2(y2)
local y3 = force2(y3)
if y1.cid == "dk_nat.O" then
local m_c, r_c = y2, y3
return app0(dk_dk_nat.O_c)
else
return nil
end
end }
dk_dk_nat.quo_aux_c = { cid="dk_nat.quo_aux" ; arity = 3 ; args = { } ; f = function(y1, y2, y3)
local y1 = force2(y1)
local y2 = force2(y2)
local y3 = force2(y3)
if y1.cid == "dk_nat.S" then
local n_c, m_c, r_c = y1.args[1], y2, y3
return app( app( app( app( app0(dk_dk_bool.ite_c) , app0(dk_dk_nat.Nat_c) ) , app( app( app0(dk_dk_nat.lt_c) , app( app0(dk_dk_nat.S_c) , r_c ) ) , m_c ) ) , app( app( app( app0(dk_dk_nat.quo_aux_c) , n_c ) , m_c ) , app( app0(dk_dk_nat.S_c) , r_c ) ) ) , app( app0(dk_dk_nat.S_c) , app( app( app( app0(dk_dk_nat.quo_aux_c) , n_c ) , m_c ) , app0(dk_dk_nat.O_c) ) ) )
else
return nil
end
end }
dk_dk_nat.quo_c = { cid = "dk_nat.quo" ; args = { } }
dk_dk_nat.quo_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_nat.N_c) ; cpi_f = function (dummy) return app0(dk_dk_nat.N_c) end } end } }
dk_dk_nat.quo_c = { cid="dk_nat.quo" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local n_c, m_c = y1, y2
return app( app( app( app0(dk_dk_nat.quo_aux_c) , n_c ) , m_c ) , app0(dk_dk_nat.O_c) )
end }
dk_dk_nat.pow_c = { cid = "dk_nat.pow" ; args = { } }
dk_dk_nat.pow_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_dk_nat.N_c) ; cpi_f = function (dummy) return app0(dk_dk_nat.N_c) end } end } }
dk_dk_nat.pow_c = { cid="dk_nat.pow" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y2.cid == "dk_nat.O" then
local n_c = y1
return app( app0(dk_dk_nat.S_c) , app0(dk_dk_nat.O_c) )
elseif y2.cid == "dk_nat.S" then
local k_c, n_c = y2.args[1], y1
return app( app( app0(dk_dk_nat.mult_c) , n_c ) , app( app( app0(dk_dk_nat.pow_c) , n_c ) , k_c ) )
else
return nil
end
end }
dk_dk_nat.Digit_c = { cid = "dk_nat.Digit" ; args = { } }
dk_dk_nat.Digit_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
dk_dk_nat._0_c = { cid = "dk_nat._0" ; args = { } }
dk_dk_nat._0_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app0(dk_dk_nat.Digit_c) ) end } }
dk_dk_nat._1_c = { cid = "dk_nat._1" ; args = { } }
dk_dk_nat._1_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app0(dk_dk_nat.Digit_c) ) end } }
dk_dk_nat._2_c = { cid = "dk_nat._2" ; args = { } }
dk_dk_nat._2_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app0(dk_dk_nat.Digit_c) ) end } }
dk_dk_nat._3_c = { cid = "dk_nat._3" ; args = { } }
dk_dk_nat._3_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app0(dk_dk_nat.Digit_c) ) end } }
dk_dk_nat._4_c = { cid = "dk_nat._4" ; args = { } }
dk_dk_nat._4_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app0(dk_dk_nat.Digit_c) ) end } }
dk_dk_nat._5_c = { cid = "dk_nat._5" ; args = { } }
dk_dk_nat._5_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app0(dk_dk_nat.Digit_c) ) end } }
dk_dk_nat._6_c = { cid = "dk_nat._6" ; args = { } }
dk_dk_nat._6_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app0(dk_dk_nat.Digit_c) ) end } }
dk_dk_nat._7_c = { cid = "dk_nat._7" ; args = { } }
dk_dk_nat._7_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app0(dk_dk_nat.Digit_c) ) end } }
dk_dk_nat._8_c = { cid = "dk_nat._8" ; args = { } }
dk_dk_nat._8_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app0(dk_dk_nat.Digit_c) ) end } }
dk_dk_nat._9_c = { cid = "dk_nat._9" ; args = { } }
dk_dk_nat._9_t = { tbox_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app0(dk_dk_nat.Digit_c) ) end } }
dk_dk_nat.__0_c = app0(dk_dk_nat.O_c)
dk_dk_nat.__0_t = dk_dk_nat.O_t
dk_dk_nat.__1_c = app( app0(dk_dk_nat.S_c) , app0(dk_dk_nat.__0_c) )
dk_dk_nat.__1_t = { tapp_f = dk_dk_nat.S_t ; tapp_a = dk_dk_nat.__0_t ; tapp_ca = { clazy = function() return app0(dk_dk_nat.__0_c) end } }
dk_dk_nat.__2_c = app( app0(dk_dk_nat.S_c) , app0(dk_dk_nat.__1_c) )
dk_dk_nat.__2_t = { tapp_f = dk_dk_nat.S_t ; tapp_a = dk_dk_nat.__1_t ; tapp_ca = { clazy = function() return app0(dk_dk_nat.__1_c) end } }
dk_dk_nat.__3_c = app( app0(dk_dk_nat.S_c) , app0(dk_dk_nat.__2_c) )
dk_dk_nat.__3_t = { tapp_f = dk_dk_nat.S_t ; tapp_a = dk_dk_nat.__2_t ; tapp_ca = { clazy = function() return app0(dk_dk_nat.__2_c) end } }
dk_dk_nat.__4_c = app( app0(dk_dk_nat.S_c) , app0(dk_dk_nat.__3_c) )
dk_dk_nat.__4_t = { tapp_f = dk_dk_nat.S_t ; tapp_a = dk_dk_nat.__3_t ; tapp_ca = { clazy = function() return app0(dk_dk_nat.__3_c) end } }
dk_dk_nat.__5_c = app( app0(dk_dk_nat.S_c) , app0(dk_dk_nat.__4_c) )
dk_dk_nat.__5_t = { tapp_f = dk_dk_nat.S_t ; tapp_a = dk_dk_nat.__4_t ; tapp_ca = { clazy = function() return app0(dk_dk_nat.__4_c) end } }
dk_dk_nat.__6_c = app( app0(dk_dk_nat.S_c) , app0(dk_dk_nat.__5_c) )
dk_dk_nat.__6_t = { tapp_f = dk_dk_nat.S_t ; tapp_a = dk_dk_nat.__5_t ; tapp_ca = { clazy = function() return app0(dk_dk_nat.__5_c) end } }
dk_dk_nat.__7_c = app( app0(dk_dk_nat.S_c) , app0(dk_dk_nat.__6_c) )
dk_dk_nat.__7_t = { tapp_f = dk_dk_nat.S_t ; tapp_a = dk_dk_nat.__6_t ; tapp_ca = { clazy = function() return app0(dk_dk_nat.__6_c) end } }
dk_dk_nat.__8_c = app( app0(dk_dk_nat.S_c) , app0(dk_dk_nat.__7_c) )
dk_dk_nat.__8_t = { tapp_f = dk_dk_nat.S_t ; tapp_a = dk_dk_nat.__7_t ; tapp_ca = { clazy = function() return app0(dk_dk_nat.__7_c) end } }
dk_dk_nat.__9_c = app( app0(dk_dk_nat.S_c) , app0(dk_dk_nat.__8_c) )
dk_dk_nat.__9_t = { tapp_f = dk_dk_nat.S_t ; tapp_a = dk_dk_nat.__8_t ; tapp_ca = { clazy = function() return app0(dk_dk_nat.__8_c) end } }
dk_dk_nat.__10_c = app( app0(dk_dk_nat.S_c) , app0(dk_dk_nat.__9_c) )
dk_dk_nat.__10_t = { tapp_f = dk_dk_nat.S_t ; tapp_a = dk_dk_nat.__9_t ; tapp_ca = { clazy = function() return app0(dk_dk_nat.__9_c) end } }
dk_dk_nat.digit_to_nat_c = { cid = "dk_nat.digit_to_nat" ; args = { } }
dk_dk_nat.digit_to_nat_t = { tbox_cty = { cpi_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app0(dk_dk_nat.Digit_c) ) end } ; cpi_f = function (dummy) return app0(dk_dk_nat.N_c) end } }
dk_dk_nat.digit_to_nat_c = { cid="dk_nat.digit_to_nat" ; arity = 1 ; args = { } ; f = function(y1)
local y1 = force2(y1)
if y1.cid == "dk_nat._0" then
return app0(dk_dk_nat.__0_c)
elseif y1.cid == "dk_nat._1" then
return app0(dk_dk_nat.__1_c)
elseif y1.cid == "dk_nat._2" then
return app0(dk_dk_nat.__2_c)
elseif y1.cid == "dk_nat._3" then
return app0(dk_dk_nat.__3_c)
elseif y1.cid == "dk_nat._4" then
return app0(dk_dk_nat.__4_c)
elseif y1.cid == "dk_nat._5" then
return app0(dk_dk_nat.__5_c)
elseif y1.cid == "dk_nat._6" then
return app0(dk_dk_nat.__6_c)
elseif y1.cid == "dk_nat._7" then
return app0(dk_dk_nat.__7_c)
elseif y1.cid == "dk_nat._8" then
return app0(dk_dk_nat.__8_c)
elseif y1.cid == "dk_nat._9" then
return app0(dk_dk_nat.__9_c)
else
return nil
end
end }
check_ext(dk_dk_list,'[ Lua ]  dk_list is undefined.')
dk_dk_nat.list_to_nat_c = { cid = "dk_nat.list_to_nat" ; args = { } }
dk_dk_nat.list_to_nat_t = { tbox_cty = { cpi_cty = { clazy = function() return app( app0(dk_cc.eT_c) , app( app0(dk_dk_list.list_c) , app0(dk_dk_nat.Digit_c) ) ) end } ; cpi_f = function (dummy) return app0(dk_dk_nat.N_c) end } }
dk_dk_nat.list_to_nat_c = { cid="dk_nat.list_to_nat" ; arity = 1 ; args = { } ; f = function(y1)
local y1 = force2(y1)
if y1.cid == "dk_list.nil" then
if y1.args[1].cid == "dk_nat.Digit" then
return app0(dk_dk_nat.O_c)
else
return nil
end
elseif y1.cid == "dk_list.cons" then
if y1.args[1].cid == "dk_nat.Digit" then
local d_c, l_c = y1.args[2], y1.args[3]
return app( app( app0(dk_dk_nat.plus_c) , app( app0(dk_dk_nat.digit_to_nat_c) , d_c ) ) , app( app( app0(dk_dk_nat.mult_c) , app0(dk_dk_nat.__10_c) ) , app( app0(dk_dk_nat.list_to_nat_c) , l_c ) ) )
else
return nil
end
else
return nil
end
end }
