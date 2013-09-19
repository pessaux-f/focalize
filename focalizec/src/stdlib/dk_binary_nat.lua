require('dedukti')
dk_binary_nat = { }
require("cc")
require("dk_nat")
check_ext(dk_cc,'[ Lua ]  cc is undefined.')
check_ext(dk_dk_nat,'[ Lua ]  dk_nat is undefined.')
dk_binary_nat.UNat_c = app( app0(dk_cc.eT_c) , app0(dk_dk_nat.Nat_c) )
dk_binary_nat.UNat_t = { tapp_f = dk_cc.eT_t ; tapp_a = dk_dk_nat.Nat_t ; tapp_ca = { clazy = function() return app0(dk_dk_nat.Nat_c) end } }
require("dk_bool")
check_ext(dk_dk_bool,'[ Lua ]  dk_bool is undefined.')
dk_binary_nat.B_c = app( app0(dk_cc.eT_c) , app0(dk_dk_bool.bool_c) )
dk_binary_nat.B_t = { tapp_f = dk_cc.eT_t ; tapp_a = dk_dk_bool.bool_t ; tapp_ca = { clazy = function() return app0(dk_dk_bool.bool_c) end } }
dk_binary_nat.BNat_c = { cid = "binary_nat.BNat" ; args = { } }
dk_binary_nat.BNat_t = { tbox_cty = { clazy = function() return app0(dk_cc.uT_c) end } }
check_ext(dk_binary_nat,'[ Lua ]  binary_nat is undefined.')
dk_binary_nat.N_c = app( app0(dk_cc.eT_c) , app0(dk_binary_nat.BNat_c) )
dk_binary_nat.N_t = { tapp_f = dk_cc.eT_t ; tapp_a = dk_binary_nat.BNat_t ; tapp_ca = { clazy = function() return app0(dk_binary_nat.BNat_c) end } }
dk_binary_nat.O_c = { cid = "binary_nat.O" ; args = { } }
dk_binary_nat.O_t = { tbox_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } }
dk_binary_nat.S0_c = { cid = "binary_nat.S0" ; args = { } }
dk_binary_nat.S0_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } ; cpi_f = function (dummy) return app0(dk_binary_nat.N_c) end } }
dk_binary_nat.S1_c = { cid = "binary_nat.S1" ; args = { } }
dk_binary_nat.S1_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } ; cpi_f = function (dummy) return app0(dk_binary_nat.N_c) end } }
dk_binary_nat.S0_c = { cid="binary_nat.S0" ; arity = 1 ; args = { } ; f = function(y1)
local y1 = force2(y1)
if y1.cid == "binary_nat.O" then
return app0(dk_binary_nat.O_c)
else
return nil
end
end }
dk_binary_nat.nat_of_bnat_c = { cid = "binary_nat.nat_of_bnat" ; args = { } }
dk_binary_nat.nat_of_bnat_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } ; cpi_f = function (dummy) return app0(dk_binary_nat.UNat_c) end } }
dk_binary_nat.nat_of_bnat_c = { cid="binary_nat.nat_of_bnat" ; arity = 1 ; args = { } ; f = function(y1)
local y1 = force2(y1)
if y1.cid == "binary_nat.O" then
return app0(dk_dk_nat.O_c)
elseif y1.cid == "binary_nat.S0" then
local bn_c = y1.args[1]
return app( app( app0(dk_dk_nat.mult_c) , app0(dk_dk_nat.__2_c) ) , app( app0(dk_binary_nat.nat_of_bnat_c) , bn_c ) )
elseif y1.cid == "binary_nat.S1" then
local bn_c = y1.args[1]
return app( app0(dk_dk_nat.S_c) , app( app( app0(dk_dk_nat.mult_c) , app0(dk_dk_nat.__2_c) ) , app( app0(dk_binary_nat.nat_of_bnat_c) , bn_c ) ) )
else
return nil
end
end }
dk_binary_nat.succ_c = { cid = "binary_nat.succ" ; args = { } }
dk_binary_nat.succ_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } ; cpi_f = function (dummy) return app0(dk_binary_nat.N_c) end } }
dk_binary_nat.succ_c = { cid="binary_nat.succ" ; arity = 1 ; args = { } ; f = function(y1)
local y1 = force2(y1)
if y1.cid == "binary_nat.O" then
return app( app0(dk_binary_nat.S1_c) , app0(dk_binary_nat.O_c) )
else
return nil
end
end }
dk_binary_nat.succ_c = { cid="binary_nat.succ" ; arity = 1 ; args = { } ; f = function(y1)
local y1 = force2(y1)
if y1.cid == "binary_nat.S0" then
local n_c = y1.args[1]
return app( app0(dk_binary_nat.S1_c) , n_c )
elseif y1.cid == "binary_nat.S1" then
local n_c = y1.args[1]
return app( app0(dk_binary_nat.S0_c) , app( app0(dk_binary_nat.succ_c) , n_c ) )
else
return nil
end
end }
dk_binary_nat.bnat_of_nat_c = { cid = "binary_nat.bnat_of_nat" ; args = { } }
dk_binary_nat.bnat_of_nat_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.UNat_c) end } ; cpi_f = function (dummy) return app0(dk_binary_nat.N_c) end } }
dk_binary_nat.bnat_of_nat_c = { cid="binary_nat.bnat_of_nat" ; arity = 1 ; args = { } ; f = function(y1)
local y1 = force2(y1)
if y1.cid == "dk_nat.O" then
return app0(dk_binary_nat.O_c)
elseif y1.cid == "dk_nat.S" then
local n_c = y1.args[1]
return app( app0(dk_binary_nat.succ_c) , app( app0(dk_binary_nat.bnat_of_nat_c) , n_c ) )
else
return nil
end
end }
dk_binary_nat.lt_c = { cid = "binary_nat.lt" ; args = { } }
dk_binary_nat.lt_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_binary_nat.N_c) ; cpi_f = function (dummy) return app0(dk_binary_nat.B_c) end } end } }
dk_binary_nat.gt_c = { cid = "binary_nat.gt" ; args = { } }
dk_binary_nat.gt_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_binary_nat.N_c) ; cpi_f = function (dummy) return app0(dk_binary_nat.B_c) end } end } }
dk_binary_nat.leq_c = { cid = "binary_nat.leq" ; args = { } }
dk_binary_nat.leq_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_binary_nat.N_c) ; cpi_f = function (dummy) return app0(dk_binary_nat.B_c) end } end } }
dk_binary_nat.geq_c = { cid = "binary_nat.geq" ; args = { } }
dk_binary_nat.geq_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_binary_nat.N_c) ; cpi_f = function (dummy) return app0(dk_binary_nat.B_c) end } end } }
dk_binary_nat.lt_c = { cid="binary_nat.lt" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y2.cid == "binary_nat.O" then
local n_c = y1
return app0(dk_dk_bool.false_c)
elseif y2.cid == "binary_nat.S0" then
if y1.cid == "binary_nat.S1" then
local n_c, m_c = y1.args[1], y2.args[1]
return app( app( app0(dk_binary_nat.lt_c) , n_c ) , m_c )
elseif y1.cid == "binary_nat.S0" then
local n_c, m_c = y1.args[1], y2.args[1]
return app( app( app0(dk_binary_nat.lt_c) , n_c ) , m_c )
elseif y1.cid == "binary_nat.O" then
local dummy_c = y2.args[1]
return app0(dk_dk_bool.true_c)
else
return nil
end
elseif y2.cid == "binary_nat.S1" then
if y1.cid == "binary_nat.S1" then
local n_c, m_c = y1.args[1], y2.args[1]
return app( app( app0(dk_binary_nat.lt_c) , n_c ) , m_c )
elseif y1.cid == "binary_nat.S0" then
local n_c, m_c = y1.args[1], y2.args[1]
return app( app( app0(dk_binary_nat.leq_c) , n_c ) , m_c )
elseif y1.cid == "binary_nat.O" then
local dummy_c = y2.args[1]
return app0(dk_dk_bool.true_c)
else
return nil
end
else
if y1.cid == "binary_nat.O" then
local m_c = y2
return app0(dk_dk_bool.true_c)
else
return nil
end
end
end }
dk_binary_nat.gt_c = { cid="binary_nat.gt" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local n_c, m_c = y1, y2
return app( app( app0(dk_binary_nat.lt_c) , m_c ) , n_c )
end }
dk_binary_nat.leq_c = { cid="binary_nat.leq" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "binary_nat.O" then
local m_c = y2
return app0(dk_dk_bool.true_c)
elseif y1.cid == "binary_nat.S0" then
if y2.cid == "binary_nat.S1" then
local m_c, n_c = y2.args[1], y1.args[1]
return app( app( app0(dk_binary_nat.leq_c) , n_c ) , m_c )
elseif y2.cid == "binary_nat.S0" then
local m_c, n_c = y2.args[1], y1.args[1]
return app( app( app0(dk_binary_nat.leq_c) , n_c ) , m_c )
elseif y2.cid == "binary_nat.O" then
local dummy_c = y1.args[1]
return app0(dk_dk_bool.false_c)
else
return nil
end
elseif y1.cid == "binary_nat.S1" then
if y2.cid == "binary_nat.S1" then
local m_c, n_c = y2.args[1], y1.args[1]
return app( app( app0(dk_binary_nat.leq_c) , n_c ) , m_c )
elseif y2.cid == "binary_nat.S0" then
local m_c, n_c = y2.args[1], y1.args[1]
return app( app( app0(dk_binary_nat.lt_c) , n_c ) , m_c )
elseif y2.cid == "binary_nat.O" then
local dummy_c = y1.args[1]
return app0(dk_dk_bool.false_c)
else
return nil
end
else
if y2.cid == "binary_nat.O" then
local n_c = y1
return app0(dk_dk_bool.false_c)
else
return nil
end
end
end }
dk_binary_nat.geq_c = { cid="binary_nat.geq" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local n_c, m_c = y1, y2
return app( app( app0(dk_binary_nat.leq_c) , m_c ) , n_c )
end }
dk_binary_nat.eq_c = { cid = "binary_nat.eq" ; args = { } }
dk_binary_nat.eq_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_binary_nat.N_c) ; cpi_f = function (dummy) return app0(dk_binary_nat.B_c) end } end } }
dk_binary_nat.eq_c = { cid="binary_nat.eq" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local n_c, m_c = y1, y2
return app( app( app0(dk_dk_bool.and_c) , app( app( app0(dk_binary_nat.leq_c) , n_c ) , m_c ) ) , app( app( app0(dk_binary_nat.geq_c) , n_c ) , m_c ) )
end }
dk_binary_nat.plus_c = { cid = "binary_nat.plus" ; args = { } }
dk_binary_nat.plus_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_binary_nat.N_c) ; cpi_f = function (dummy) return app0(dk_binary_nat.N_c) end } end } }
dk_binary_nat.plus_c = { cid="binary_nat.plus" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "binary_nat.O" then
local m_c = y2
return m_c
elseif y1.cid == "binary_nat.S0" then
if y2.cid == "binary_nat.S1" then
local m_c, n_c = y2.args[1], y1.args[1]
return app( app0(dk_binary_nat.S1_c) , app( app( app0(dk_binary_nat.plus_c) , n_c ) , m_c ) )
elseif y2.cid == "binary_nat.S0" then
local m_c, n_c = y2.args[1], y1.args[1]
return app( app0(dk_binary_nat.S0_c) , app( app( app0(dk_binary_nat.plus_c) , n_c ) , m_c ) )
elseif y2.cid == "binary_nat.O" then
local dummy_c = y1.args[1]
return n_c
else
return nil
end
elseif y1.cid == "binary_nat.S1" then
if y2.cid == "binary_nat.S1" then
local m_c, n_c = y2.args[1], y1.args[1]
return app( app0(dk_binary_nat.S0_c) , app( app0(dk_binary_nat.succ_c) , app( app( app0(dk_binary_nat.plus_c) , n_c ) , m_c ) ) )
elseif y2.cid == "binary_nat.S0" then
local m_c, n_c = y2.args[1], y1.args[1]
return app( app0(dk_binary_nat.S1_c) , app( app( app0(dk_binary_nat.plus_c) , n_c ) , m_c ) )
elseif y2.cid == "binary_nat.O" then
local dummy_c = y1.args[1]
return n_c
else
return nil
end
else
if y2.cid == "binary_nat.O" then
local n_c = y1
return n_c
else
return nil
end
end
end }
dk_binary_nat.mult_c = { cid = "binary_nat.mult" ; args = { } }
dk_binary_nat.mult_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_binary_nat.N_c) ; cpi_f = function (dummy) return app0(dk_binary_nat.N_c) end } end } }
dk_binary_nat.mult_c = { cid="binary_nat.mult" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "binary_nat.O" then
local m_c = y2
return app0(dk_binary_nat.O_c)
elseif y1.cid == "binary_nat.S0" then
if y2.cid == "binary_nat.S1" then
local m_c, n_c = y2.args[1], y1.args[1]
return app( app0(dk_binary_nat.S0_c) , app( app( app0(dk_binary_nat.plus_c) , m_c ) , app( app0(dk_binary_nat.S0_c) , app( app( app0(dk_binary_nat.mult_c) , n_c ) , m_c ) ) ) )
elseif y2.cid == "binary_nat.S0" then
local m_c, n_c = y2.args[1], y1.args[1]
return app( app0(dk_binary_nat.S0_c) , app( app0(dk_binary_nat.S0_c) , app( app( app0(dk_binary_nat.mult_c) , n_c ) , m_c ) ) )
elseif y2.cid == "binary_nat.O" then
local dummy_c = y1.args[1]
return app0(dk_binary_nat.O_c)
else
return nil
end
elseif y1.cid == "binary_nat.S1" then
if y2.cid == "binary_nat.S1" then
local m_c, n_c = y2.args[1], y1.args[1]
return app( app0(dk_binary_nat.S1_c) , app( app( app0(dk_binary_nat.plus_c) , app( app0(dk_binary_nat.S0_c) , app( app( app0(dk_binary_nat.mult_c) , m_c ) , n_c ) ) ) , app( app( app0(dk_binary_nat.plus_c) , n_c ) , m_c ) ) )
elseif y2.cid == "binary_nat.S0" then
local m_c, n_c = y2.args[1], y1.args[1]
return app( app0(dk_binary_nat.S0_c) , app( app( app0(dk_binary_nat.plus_c) , n_c ) , app( app0(dk_binary_nat.S0_c) , app( app( app0(dk_binary_nat.mult_c) , n_c ) , m_c ) ) ) )
elseif y2.cid == "binary_nat.O" then
local dummy_c = y1.args[1]
return app0(dk_binary_nat.O_c)
else
return nil
end
else
if y2.cid == "binary_nat.O" then
local n_c = y1
return app0(dk_binary_nat.O_c)
else
return nil
end
end
end }
dk_binary_nat.max_c = { cid = "binary_nat.max" ; args = { } }
dk_binary_nat.max_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_binary_nat.N_c) ; cpi_f = function (dummy) return app0(dk_binary_nat.N_c) end } end } }
dk_binary_nat.max_c = { cid="binary_nat.max" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local m_c, n_c = y1, y2
return app( app( app( app( app0(dk_dk_bool.ite_c) , app0(dk_binary_nat.BNat_c) ) , app( app( app0(dk_binary_nat.leq_c) , m_c ) , n_c ) ) , n_c ) , m_c )
end }
dk_binary_nat.min_c = { cid = "binary_nat.min" ; args = { } }
dk_binary_nat.min_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_binary_nat.N_c) ; cpi_f = function (dummy) return app0(dk_binary_nat.N_c) end } end } }
dk_binary_nat.min_c = { cid="binary_nat.min" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local m_c, n_c = y1, y2
return app( app( app( app( app0(dk_dk_bool.ite_c) , app0(dk_binary_nat.BNat_c) ) , app( app( app0(dk_binary_nat.leq_c) , m_c ) , n_c ) ) , m_c ) , n_c )
end }
dk_binary_nat.div2_c = { cid = "binary_nat.div2" ; args = { } }
dk_binary_nat.div2_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } ; cpi_f = function (dummy) return app0(dk_binary_nat.N_c) end } }
dk_binary_nat.div2_c = { cid="binary_nat.div2" ; arity = 1 ; args = { } ; f = function(y1)
local y1 = force2(y1)
if y1.cid == "binary_nat.O" then
return app0(dk_binary_nat.O_c)
elseif y1.cid == "binary_nat.S0" then
local n_c = y1.args[1]
return n_c
elseif y1.cid == "binary_nat.S1" then
local n_c = y1.args[1]
return n_c
else
return nil
end
end }
dk_binary_nat.length_c = { cid = "binary_nat.length" ; args = { } }
dk_binary_nat.length_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } ; cpi_f = function (dummy) return app0(dk_binary_nat.UNat_c) end } }
dk_binary_nat.length_c = { cid="binary_nat.length" ; arity = 1 ; args = { } ; f = function(y1)
local y1 = force2(y1)
if y1.cid == "binary_nat.O" then
return app0(dk_dk_nat.O_c)
elseif y1.cid == "binary_nat.S0" then
local n_c = y1.args[1]
return app( app0(dk_dk_nat.S_c) , app( app0(dk_binary_nat.length_c) , n_c ) )
elseif y1.cid == "binary_nat.S1" then
local n_c = y1.args[1]
return app( app0(dk_dk_nat.S_c) , app( app0(dk_binary_nat.length_c) , n_c ) )
else
return nil
end
end }
dk_binary_nat.quo2_c = { cid = "binary_nat.quo2" ; args = { } }
dk_binary_nat.quo2_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_binary_nat.UNat_c) ; cpi_f = function (dummy) return app0(dk_binary_nat.N_c) end } end } }
dk_binary_nat.quo2_c = { cid="binary_nat.quo2" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y2.cid == "dk_nat.O" then
local n_c = y1
return n_c
elseif y2.cid == "dk_nat.S" then
if y1.cid == "binary_nat.S1" then
local n_c, k_c = y1.args[1], y2.args[1]
return app( app( app0(dk_binary_nat.quo2_c) , n_c ) , k_c )
elseif y1.cid == "binary_nat.S0" then
local n_c, k_c = y1.args[1], y2.args[1]
return app( app( app0(dk_binary_nat.quo2_c) , n_c ) , k_c )
elseif y1.cid == "binary_nat.O" then
local dummy_c = y2.args[1]
return app0(dk_binary_nat.O_c)
else
return nil
end
else
if y1.cid == "binary_nat.O" then
local k_c = y2
return app0(dk_binary_nat.O_c)
else
return nil
end
end
end }
dk_binary_nat.mod2_c = { cid = "binary_nat.mod2" ; args = { } }
dk_binary_nat.mod2_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_binary_nat.N_c) end } ; cpi_f = function (dummy) return { cpi_cty = app0(dk_binary_nat.UNat_c) ; cpi_f = function (dummy) return app0(dk_binary_nat.N_c) end } end } }
dk_binary_nat.mod2_c = { cid="binary_nat.mod2" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y2.cid == "dk_nat.O" then
local n_c = y1
return app0(dk_binary_nat.O_c)
elseif y2.cid == "dk_nat.S" then
if y1.cid == "binary_nat.S1" then
local n_c, k_c = y1.args[1], y2.args[1]
return app( app0(dk_binary_nat.S1_c) , app( app( app0(dk_binary_nat.mod2_c) , n_c ) , k_c ) )
elseif y1.cid == "binary_nat.S0" then
local n_c, k_c = y1.args[1], y2.args[1]
return app( app0(dk_binary_nat.S0_c) , app( app( app0(dk_binary_nat.mod2_c) , n_c ) , k_c ) )
elseif y1.cid == "binary_nat.O" then
local dummy_c = y2.args[1]
return app0(dk_binary_nat.O_c)
else
return nil
end
else
if y1.cid == "binary_nat.O" then
local k_c = y2
return app0(dk_binary_nat.O_c)
else
return nil
end
end
end }
